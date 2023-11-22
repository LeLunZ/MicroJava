package ssw.mj.impl;

import ssw.mj.Errors;
import ssw.mj.Errors.Message;
import ssw.mj.scanner.Token;

import java.util.EnumSet;

import static ssw.mj.scanner.Token.Kind.none;

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
  private static final EnumSet<Token.Kind> firstStatement = EnumSet.of(Token.Kind.ident, Token.Kind.if_, Token.Kind.while_, Token.Kind.break_, Token.Kind.return_, Token.Kind.read, Token.Kind.print, Token.Kind.lbrace, Token.Kind.semicolon);
  private static final EnumSet<Token.Kind> firstAssingop = EnumSet.of(Token.Kind.assign, Token.Kind.plusas, Token.Kind.minusas, Token.Kind.timesas, Token.Kind.slashas, Token.Kind.remas);
  private static final EnumSet<Token.Kind> firstExpr = EnumSet.of(Token.Kind.minus, Token.Kind.ident, Token.Kind.number, Token.Kind.charConst, Token.Kind.new_, Token.Kind.lpar);
  private static final EnumSet<Token.Kind> firstRelop = EnumSet.of(Token.Kind.eql, Token.Kind.neq, Token.Kind.gtr, Token.Kind.geq, Token.Kind.lss, Token.Kind.leq);
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

  // ===============================================
  // TODO Exercise 3: Error recovery methods
  // TODO Exercise 4: Symbol table handling
  // TODO Exercise 5-6: Code generation
  // ===============================================

  // TODO Exercise 3: Error distance

  // TODO Exercise 2 + Exercise 3: Sets to handle certain first, follow, and recover sets

  // ---------------------------------

  /**
   * Reads ahead one symbol.
   */
  private void scan() {
    t = la;
    la = scanner.next();
    sym = la.kind;
  }

  /**
   * Verifies symbol and reads ahead.
   */
  private void check(Token.Kind expected) {
    if (sym == expected) {
      scan();
    } else {
      error(Errors.Message.TOKEN_EXPECTED, expected);
    }
  }

  /**
   * Adds error message to the list of errors.
   */
  public void error(Message msg, Object... msgParams) {
    // TODO Exercise 3: Replace panic mode with error recovery (i.e., keep track of error distance)
    // TODO Exercise 3: Hint: Replacing panic mode also affects scan() method
    scanner.errors.error(la.line, la.col, msg, msgParams);
    throw new Errors.PanicMode();
  }

  /**
   * Starts the analysis.
   */
  public void parse() {
    scan();
    Program();
    check(Token.Kind.eof);
  }

  private void Program() {
    check(Token.Kind.program);
    check(Token.Kind.ident);

    while (sym != Token.Kind.lbrace) {
      switch (sym) {
        case class_ -> ClassDecl();
        case ident -> VarDecl();
        case final_ -> ConstDecl();
      }
    }

    check(Token.Kind.lbrace);
    while (sym != Token.Kind.rbrace) {
      MethodDecl();
    }
    check(Token.Kind.rbrace);
  }

  private void MethodDecl() {
    switch (sym) {
      case void_ -> scan();
      case ident -> Type();
      default -> error(Errors.Message.INVALID_METH_DECL);
    }


    check(Token.Kind.ident);
    check(Token.Kind.lpar);
    while (sym != Token.Kind.rpar) {
      FormPars();
    }

    check(Token.Kind.rpar);
    while (sym != Token.Kind.lbrace) {
      VarDecl();
    }
    Block();
  }

  void Type() {
    check(Token.Kind.ident);
    if (sym == Token.Kind.lbrack) {
      scan();
      check(Token.Kind.rbrack);
    }
  }

  private void ClassDecl() {
    check(Token.Kind.class_);
    check(Token.Kind.ident);
    check(Token.Kind.lbrace);
    while (sym != Token.Kind.rbrace) {
      VarDecl();
    }
    check(Token.Kind.rbrace);
  }

  void ConstDecl() {
    check(Token.Kind.final_);
    Type();
    check(Token.Kind.ident);
    check(Token.Kind.assign);
    switch (sym) {
      case number, charConst -> scan();
      default -> error(Errors.Message.CONST_DECL);
    }
    check(Token.Kind.semicolon);
  }

  private void VarDecl() {
    Type();
    check(Token.Kind.ident);
    while (sym == Token.Kind.comma) {
      scan();
      check(Token.Kind.ident);
    }
    check(Token.Kind.semicolon);
  }

  private void FormPars() {
    Type();
    check(Token.Kind.ident);
    while (sym == Token.Kind.comma) {
      scan();
      Type();
      check(Token.Kind.ident);
    }
  }

  private void Block() {
    check(Token.Kind.lbrace);
    while (firstStatement.contains(sym)) {
      Statement();
    }
    check(Token.Kind.rbrace);
  }

  private void Assignop() {
    // This can also be a long switch with multiple cases
    // where we check for each assignop
    // this is just way shorter
    // I think performance wise it should be equal as we are using a Set

    if (firstAssingop.contains(sym)) {
      scan();
    } else {
      error(Errors.Message.ASSIGN_OP);
    }
  }

  private void Relop() {
    if (firstRelop.contains(sym)) {
      scan();
    } else {
      error(Errors.Message.REL_OP);
    }
  }

  private void Statement() {
    switch (sym) {
      case ident -> {
        Designator();
        if (firstAssingop.contains(sym)) {
          Assignop();
          Expr();
        } else {
          switch (sym) {
            case lpar -> ActPars();
            case pplus, mminus -> scan();
            default -> error(Errors.Message.DESIGN_FOLLOW);
          }
        }
        check(Token.Kind.semicolon);
      }
      case if_ -> {
        scan();
        check(Token.Kind.lpar);
        Condition();
        check(Token.Kind.rpar);
        Statement();
        if (sym == Token.Kind.else_) {
          scan();
          Statement();
        }
      }
      case while_ -> {
        scan();
        check(Token.Kind.lpar);
        Condition();
        check(Token.Kind.rpar);
        Statement();
      }
      case break_ -> {
        scan();
        check(Token.Kind.semicolon);
      }
      case return_ -> {
        scan();
        if (firstExpr.contains(sym)) {
          Expr();
        }
        check(Token.Kind.semicolon);
      }
      case read -> {
        scan();
        check(Token.Kind.lpar);
        Designator();
        check(Token.Kind.rpar);
        check(Token.Kind.semicolon);
      }
      case print -> {
        scan();
        check(Token.Kind.lpar);
        Expr();
        if (sym == Token.Kind.comma) {
          scan();
          check(Token.Kind.number);
        }
        check(Token.Kind.rpar);
        check(Token.Kind.semicolon);
      }
      case lbrace -> Block();
      case semicolon -> scan();
      default -> error(Errors.Message.INVALID_STAT);
    }
  }

  private void Expr() {
    if (sym == Token.Kind.minus) {
      scan();
    }
    Term();

    while (sym == Token.Kind.plus || sym == Token.Kind.minus) {
      Addop();
      Term();
    }
  }

  private void Term() {
    Factor();
    boolean isMulop = true;
    while (isMulop) {
      switch (sym) {
        case times, slash, rem -> {
          Mulop();
          Factor();
        }
        case exp -> {
          scan();
          check(Token.Kind.number);
        }
        default -> isMulop = false;
      }
    }
  }

  private void Condition() {
    CondTerm();
    while (sym == Token.Kind.or) {
      scan();
      CondTerm();
    }
  }


  private void CondTerm() {
    CondFact();
    while (sym == Token.Kind.and) {
      scan();
      CondFact();
    }
  }

  private void CondFact() {
    Expr();
    Relop();
    Expr();
  }

  private void Factor() {
    switch (sym) {
      case ident -> {
        Designator();
        if (sym == Token.Kind.lpar) {
          ActPars();
        }
      }
      case new_ -> {
        scan();
        check(Token.Kind.ident);
        if (sym == Token.Kind.lbrack) {
          scan();
          Expr();
          check(Token.Kind.rbrack);
        }
      }
      case lpar -> {
        scan();
        Expr();
        check(Token.Kind.rpar);
      }
      case number, charConst -> scan();
      default -> error(Errors.Message.INVALID_FACT);
    }
  }

  private void ActPars() {
    check(Token.Kind.lpar);
    if (firstExpr.contains(sym)) {
      Expr();

      while (sym == Token.Kind.comma) {
        scan();
        Expr();
      }
    }
    check(Token.Kind.rpar);
  }

  private void Designator() {
    check(Token.Kind.ident);
    boolean isDesignator = true;
    while (isDesignator) {
      switch (sym) {
        case period -> {
          scan();
          check(Token.Kind.ident);
        }
        case lbrack -> {
          scan();
          Expr();
          check(Token.Kind.rbrack);
        }
        default -> isDesignator = false;
      }
    }
  }

  private void Addop() {
    switch (sym) {
      case plus, minus -> scan();
      default -> error(Errors.Message.ADD_OP);
    }
  }

  private void Mulop() {
    switch (sym) {
      case times, slash, rem -> scan();
      default -> error(Errors.Message.MUL_OP);
    }
  }

  // ------------------------------------

  // TODO Exercise 3: Error recovery methods: recoverDecl, recoverMethodDecl and recoverStat

  // ====================================
  // ====================================
}
