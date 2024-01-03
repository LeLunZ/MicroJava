package ssw.mj.impl;

import ssw.mj.Errors;
import ssw.mj.Errors.Message;
import ssw.mj.codegen.Operand;
import ssw.mj.scanner.Token;
import ssw.mj.symtab.Obj;
import ssw.mj.symtab.Struct;

import java.util.EnumSet;

import static ssw.mj.Errors.Message.*;
import static ssw.mj.scanner.Token.Kind.*;

public final class Parser {
  /**
   * Maximum number of global variables per program
   */
  private static final int MAX_GLOBALS = 32767;
  /**
   * Maximum number of fields per class
   */
  private static final int MAX_FIELDS = 32767;
  /**
   * Maximum number of local variables per method
   */
  private static final int MAX_LOCALS = 127;
  // definition of beginning tokens for specific sets
  private static final EnumSet<Token.Kind> firstStatement = EnumSet.of(ident, if_, while_, break_, return_, read, print, lbrace, semicolon);
  private static final EnumSet<Token.Kind> firstAssingop = EnumSet.of(assign, plusas, minusas, timesas, slashas, remas);
  private static final EnumSet<Token.Kind> firstExpr = EnumSet.of(minus, ident, number, charConst, new_, lpar);
  private static final EnumSet<Token.Kind> firstRelop = EnumSet.of(eql, neq, gtr, geq, lss, leq);
  private static final EnumSet<Token.Kind> firstMulop = EnumSet.of(times, slash, rem);
  private static final EnumSet<Token.Kind> followSymbolMeth = EnumSet.of(rbrace, eof);
  private static final EnumSet<Token.Kind> followSymbolDecl = EnumSet.of(lbrace, eof);
  private static final EnumSet<Token.Kind> followSymbolBlock = EnumSet.of(rbrace, eof);
  private static final EnumSet<Token.Kind> recoverSymbolStat = EnumSet.of(if_, while_, break_, return_, read, print, semicolon, eof);
  private static final EnumSet<Token.Kind> recoverSymbolDecl = EnumSet.of(final_, ident, class_, lbrace, eof);
  private static final EnumSet<Token.Kind> recoverSymbolMeth = EnumSet.of(void_, ident, eof);

  /**
   * According scanner
   */
  public final Scanner scanner;
  /**
   * According code buffer
   */
  public final Code code;
  /**
   * According symbol table
   */
  public final Tab tab;
  private final int ERROR_DISTANCE = 3;
  private final int DEFAULT_WIDTH = 1;

  // ===============================================
  // TODO Exercise 4: Symbol table handling
  // TODO Exercise 5-6: Code generation
  // ===============================================
  private int errDist = ERROR_DISTANCE;
  /**
   * Last recognized token;
   */
  private Token t;
  /**
   * Lookahead token (not recognized).)
   */
  private Token la;
  /**
   * Shortcut to kind attribute of lookahead token (la).
   */
  private Token.Kind sym;

  public Parser(Scanner scanner) {
    this.scanner = scanner;
    tab = new Tab(this);
    code = new Code(this);
    // Pseudo token to avoid crash when 1st symbol has scanner error.
    la = new Token(none, 1, 1);
  }

  /**
   * Reads ahead one symbol.
   */
  private void scan() {
    t = la;
    la = scanner.next();
    sym = la.kind;
    errDist++;
  }

  /**
   * Verifies symbol and reads ahead.
   */
  private void check(Token.Kind expected) {
    if (sym == expected) {
      scan();
    } else {
      error(TOKEN_EXPECTED, expected);
    }
  }

  /**
   * Adds error message to the list of errors.
   */
  public void error(Message msg, Object... msgParams) {
    if (errDist >= ERROR_DISTANCE) {
      scanner.errors.error(la.line, la.col, msg, msgParams);
    }
    errDist = 0;
  }

  /**
   * Starts the analysis.
   */
  public void parse() {
    scan();
    Program();
    check(eof);
    code.dataSize = code.pc;
  }

  private void Program() {
    check(program);
    check(ident);

    final Obj program = tab.insert(Obj.Kind.Prog, t.val, Tab.noType);
    tab.openScope();

    while (!followSymbolDecl.contains(sym)) {
      switch (sym) {
        case class_ -> ClassDecl();
        case ident -> VarDecl();
        case final_ -> ConstDecl();
        default -> {
          error(INVALID_DECL);
          recoverDecl();
        }
      }
    }

    code.dataSize = tab.curScope.nVars();

    if (tab.curScope.nVars() > MAX_GLOBALS) {
      error(TOO_MANY_GLOBALS);
    }

    check(lbrace);
    while (!followSymbolMeth.contains(sym)) {
      if (sym != void_ && sym != ident) {
        error(INVALID_METH_DECL);
        recoverMethodDecl();
      } else {
        MethodDecl();
      }
    }

    check(rbrace);

    if(code.mainpc < 0) {
      error(METH_NOT_FOUND, "main");
    }

    program.locals = tab.curScope.locals();

    tab.closeScope();
  }

  private void MethodDecl() {
    Struct type = Tab.noType;
    switch (sym) {
      case void_ -> scan();
      case ident -> type = Type();
      default -> {
        error(INVALID_METH_DECL);
      }
    }


    check(ident);

    final String methodName = t.val;
    final Obj meth = tab.insert(Obj.Kind.Meth, methodName, type);
    meth.adr = code.pc;

    if (methodName.equals("main")) {
      code.mainpc = meth.adr;
    }

    check(lpar);
    tab.openScope();

    while (sym == ident) {
      FormPars();
    }

    meth.nPars = tab.curScope.nVars();

    check(rpar);

    if ("main".equals(methodName)) {
      if (meth.nPars != 0) {
        error(MAIN_WITH_PARAMS);
      }
      if (meth.type != Tab.noType) {
        error(MAIN_NOT_VOID);
      }
    }

    while (sym == ident) {
      VarDecl();
    }

    if (tab.curScope.nVars() > MAX_LOCALS) {
      error(TOO_MANY_LOCALS);
    }

    code.put(Code.OpCode.enter);
    code.put(meth.nPars);
    code.put(tab.curScope.nVars());

    Block();

    meth.locals = tab.curScope.locals();

    if (type == Tab.noType) {
      code.put(Code.OpCode.exit);
      code.put(Code.OpCode.return_);
    } else {
      code.put(Code.OpCode.trap);
      code.put(1);
    }


    tab.closeScope();
  }

  private Struct Type() {
    check(ident);
    Obj o = tab.find(t.val);
    if (o.kind != Obj.Kind.Type) {
      error(NO_TYPE);
    }

    Struct type = o.type;
    if (sym == lbrack) {
      scan();
      check(rbrack);
      type = new Struct(type);
    }
    return type;
  }

  private void ClassDecl() {
    check(class_);
    check(ident);

    final Obj classObject = tab.insert(Obj.Kind.Type, t.val, new Struct(Struct.Kind.Class));

    check(lbrace);
    tab.openScope();

    while (sym == ident) {
      VarDecl();
    }

    if (tab.curScope.nVars() > MAX_FIELDS) {
      error(TOO_MANY_FIELDS);
    }
    classObject.type.fields = tab.curScope.locals();

    tab.closeScope();
    check(rbrace);
  }

  void ConstDecl() {
    check(final_);
    Struct type = Type();
    check(ident);
    final String constName = t.val;
    check(assign);

    Obj constObj = tab.insert(Obj.Kind.Con, constName, type);

    switch (sym) {
      case number -> {
        if (type.kind != Tab.intType.kind) {
          error(CONST_TYPE);
        }
        scan();
        constObj.val = t.numVal;
      }
      case charConst -> {
        if (type.kind != Tab.charType.kind) {
          error(CONST_TYPE);
        }
        scan();
        constObj.val = t.numVal;
      }
      default -> error(CONST_DECL);
    }
    check(semicolon);
  }

  private void VarDecl() {
    Struct type = Type();
    check(ident);

    tab.insert(Obj.Kind.Var, t.val, type);
    while (sym == comma) {
      scan();
      check(ident);
      tab.insert(Obj.Kind.Var, t.val, type);
    }
    check(semicolon);
  }

  private void FormPars() {
    // changed to optimized variant
    for (; ; ) {
      Struct type = Type();
      check(ident);
      tab.insert(Obj.Kind.Var, t.val, type);
      if (sym != comma) {
        break;
      }
      scan();
    }
  }

  private void Block() {
    check(lbrace);
    tab.openScope();

    while (!followSymbolBlock.contains(sym)) {
      Statement();
    }
    check(rbrace);
    tab.closeScope();
  }

  private Code.OpCode Assignop() {
    switch (sym){
      case assign -> {
        scan();
        return Code.OpCode.store;
      }
      case plusas -> {
        scan();
        return Code.OpCode.add;
      }
      case minusas -> {
        scan();
        return Code.OpCode.sub;
      }
      case timesas -> {
        scan();
        return Code.OpCode.mul;
      }
      case slashas -> {
        scan();
        return Code.OpCode.div;
      }
      case remas -> {
        scan();
        return Code.OpCode.rem;
      }
      default -> {
        error(ASSIGN_OP);
        return Code.OpCode.nop;
      }
    }
  }

  private void Relop() {
    if (firstRelop.contains(sym)) {
      scan();
    } else {
      error(REL_OP);
    }
  }

  private void Statement() {
    switch (sym) {
      case ident -> {
        Operand x = Designator();

        if (firstAssingop.contains(sym)) {
          if (!x.canBeAssignedTo()) {
            error(CANNOT_ASSIGN_TO, x.kind);
          }

          if(x.kind == Operand.Kind.Meth){
            error(INVALID_CALL);
          }
          final Code.OpCode operator = Assignop();

          if(operator != Code.OpCode.store && (x.kind == Operand.Kind.Meth || x.kind == Operand.Kind.Cond)){
            error(NO_VAL);
          }

          if (operator != Code.OpCode.store) {
            code.compoundAssignmentPrepare(x);
          }

          Operand y = Expr();

          if (x.obj != null && x.obj.kind != Obj.Kind.Var) {
            error(NO_VAL);
          }

          if(operator == Code.OpCode.store){
            if(y.type.assignableTo(x.type)){
              code.assign(x, y);
            } else {
              error(INCOMP_TYPES);
            }
          } else {
            if (x.type != Tab.intType || y.type != Tab.intType) {
              error(NO_INT_OPERAND);
            }
            code.load(y);
            code.put(operator);
            code.store(x);
          }

        } else if (sym == lpar) {
          if (x.kind != Operand.Kind.Meth) {
            error(Errors.Message.NO_METH);
          }

          ActPars();

          if (x.type != Tab.noType) {
            code.put(Code.OpCode.pop);
          }
        } else if (sym == pplus || sym == mminus) {
          if (!x.canBeAssignedTo()) {
            error(CANNOT_ASSIGN_TO, x.kind);
          }

          if(x.kind == Operand.Kind.Meth){
            error(INVALID_CALL);
          }

          if (x.obj != null && x.obj.kind != Obj.Kind.Var) {
            error(NO_INT_OPERAND);
          }

          if (x.type != Tab.intType) {
            error(NO_INT_OPERAND);
          }

          code.inc(x, sym == pplus ? 1 : -1);

          scan();
        } else {
          error(DESIGN_FOLLOW);
        }
        check(semicolon);
      }
      case if_ -> {
        scan();
        check(lpar);
        Condition();
        check(rpar);
        Statement();
        if (sym == else_) {
          scan();
          Statement();
        }
      }
      case while_ -> {
        scan();
        check(lpar);
        Condition();
        check(rpar);
        Statement();
      }
      case break_ -> {
        scan();
        check(semicolon);
      }
      case return_ -> {
        scan();
        if (firstExpr.contains(sym)) {
          Operand o = Expr();
          code.load(o);
        }
        code.put(Code.OpCode.exit);
        code.put(Code.OpCode.return_);
        check(semicolon);
      }
      case read -> {
        scan();
        check(lpar);
        Operand o = Designator();

        if (o.type == Tab.intType){
          code.put(Code.OpCode.read);
          code.store(o);
        } else if (o.type == Tab.charType) {
          code.put(Code.OpCode.bread);
          code.store(o);
        } else {
          error(READ_VALUE);
        }

        check(rpar);
        check(semicolon);
      }
      case print -> {
        scan();
        check(lpar);
        Operand x = Expr();
        int width = DEFAULT_WIDTH;

        if (sym == comma) {
          scan();
          check(number);
          width = t.numVal;
        }

        if (x.type == Tab.intType) {
          code.load(x);
          code.loadConst(0);
          code.put(Code.OpCode.print);
        } else if (x.type == Tab.charType) {
          code.load(x);
          code.loadConst(width);
          code.put(Code.OpCode.bprint);
        } else {
          error(Errors.Message.PRINT_VALUE);
        }

        check(rpar);
        check(semicolon);
      }
      case lbrace -> Block();
      case semicolon -> scan();
      default -> {
        error(INVALID_STAT);
        recoverStat();
      }
    }
  }

  private Operand Expr() {
    boolean neg = false;
    if (sym == minus) {
      scan();
      neg = true;
    }
    Operand x = Term();

    if (neg) {
      if (x.type != Tab.intType) {
        error(NO_INT_OPERAND);
      }

      if(x.kind == Operand.Kind.Con){
        x.val = -x.val;
      } else {
        code.load(x);
        code.put(Code.OpCode.neg);
      }
    }

    while (sym == plus || sym == minus) {
      Code.OpCode op = Addop();
      Operand y = Term();

      if (x.type != Tab.intType || y.type != Tab.intType) {
        error(NO_INT_OPERAND);
      }

      code.load(y);
      code.put(op);
    }

    return x;
  }

  private Operand Term() {
    Operand x = Factor();
    code.load(x);

    while (firstMulop.contains(sym) || sym == exp) {
      if (sym == exp) {
        scan();
        check(number);

        if (t.numVal == 0) {
          code.loadConst(1);
        }else{
          for (int i = 0; i < t.numVal - 1; i++) {
            code.put(Code.OpCode.dup);
          }
          for (int i = 0; i < t.numVal - 1; i++) {
            code.put(Code.OpCode.mul);
          }
        }

        if (x.type != Tab.intType) {
          error(NO_INT_OPERAND);
        }
      } else {
        Code.OpCode op = Mulop();
        Operand y = Factor();

        if(y.kind == Operand.Kind.Meth){
          error(NO_VAL);
        }

        if (x.type != Tab.intType || y.type != Tab.intType) {
          error(NO_INT_OPERAND);
        }

        code.load(y);
        code.put(op);
      }
    }
    return x;
  }

  private void Condition() {
    CondTerm();
    while (sym == or) {
      scan();
      CondTerm();
    }
  }

  private void CondTerm() {
    CondFact();
    while (sym == and) {
      scan();
      CondFact();
    }
  }

  private void CondFact() {
    Expr();
    Relop();
    Expr();
  }

  private Operand Factor() {
    Operand x;
    switch (sym) {
      case ident -> {
        x = Designator();
        if (sym == lpar) {
          if( x.kind != Operand.Kind.Meth){
            error(NO_METH);
          }

          if(x.obj.type == Tab.noType){
            error(INVALID_CALL);
          }

          code.methodCall(x);
          x.kind = Operand.Kind.Stack;
          ActPars();
        }
      }
      case new_ -> {
        scan();
        check(ident);
        Obj obj = tab.find(t.val);

        if(obj.kind != Obj.Kind.Type){
          error(NO_TYPE);
        }

        Struct type = obj.type;

        if (sym == lbrack) {
          scan();
          Operand arrSize = Expr();
          if (arrSize.type != Tab.intType) {
            error(ARRAY_SIZE);
          }

          code.load(arrSize);
          code.put(Code.OpCode.newarray);

          if(type == Tab.charType){
            code.put(0);
          } else {
            code.put(1);
          }

          type = new Struct(type);

          check(rbrack);
        }else if (type.kind == Struct.Kind.Class) {
          code.put(Code.OpCode.new_);
          code.put2(type.nrFields());
        } else {
          error(NO_CLASS_TYPE);
        }

        x = new Operand(type);
      }
      case lpar -> {
        scan();
        x = Expr();
        check(rpar);
      }
      case number -> {
        scan();
        x = new Operand(t.numVal);
        x.type = Tab.intType;
      }
      case charConst -> {
        scan();
        x = new Operand(t.numVal);
        x.type = Tab.charType;
      }
      default -> {
        error(INVALID_FACT);
        x = new Operand(Tab.noType);
      }
    }
    return x;
  }

  private void ActPars() {
    check(lpar);
    if (firstExpr.contains(sym)) {
      Expr();

      while (sym == comma) {
        scan();
        Expr();
      }
    }
    check(rpar);
  }

  private Operand Designator() {
    check(ident);
    Operand x = new Operand(tab.find(t.val), this);
    boolean isDesignator = true;
    while (isDesignator) {
      switch (sym) {
        case period -> {
          if(x.type.kind != Struct.Kind.Class){
            error(NO_CLASS);
          }
          scan();
          code.load(x);
          check(ident);

          Obj obj = tab.findField(t.val, x.type);
          x.kind = Operand.Kind.Fld;
          x.type = obj.type;
          x.adr = obj.adr;
        }
        case lbrack -> {
          code.load(x);

          scan();
          Operand i = Expr();

          if (x.obj != null || x.type.kind != Struct.Kind.Arr) {
            error(NO_ARRAY);
          }

          if (i.type != Tab.intType) {
            error(ARRAY_INDEX);
          }

          code.load(i);

          x.kind = Operand.Kind.Elem;
          x.type = x.type.elemType;
          check(rbrack);
        }
        default -> isDesignator = false;
      }
    }
    return x;
  }

  private Code.OpCode Addop() {
    switch (sym) {
      case plus -> {
        scan();
        return Code.OpCode.add;
      }
      case minus -> {
        scan();
        return Code.OpCode.sub;
      }
      default -> error(ADD_OP);
    }
    return Code.OpCode.nop;
  }

  private Code.OpCode Mulop() {
    switch (sym) {
      case times -> {
        scan();
        return Code.OpCode.mul;
      }
      case slash -> {
        scan();
        return Code.OpCode.div;
      }
      case rem -> {
        scan();
        return Code.OpCode.rem;
      }
      default -> error(MUL_OP);
    }
    return Code.OpCode.nop;
  }

  private void recoverStat() {
    do {
      scan();
    } while (!recoverSymbolStat.contains(sym));
    errDist = 0;
  }

  private void recoverDecl() {
    do {
      scan();
    } while (!recoverSymbolDecl.contains(sym));
    errDist = 0;
  }

  private void recoverMethodDecl() {
    do {
      scan();
    } while (!recoverSymbolMeth.contains(sym));
    errDist = 0;
  }
}
