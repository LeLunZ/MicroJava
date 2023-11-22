package ssw.mj.impl;

import ssw.mj.Errors.Message;
import ssw.mj.scanner.Token;

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
  private static final EnumSet<Token.Kind> recoverSymbolDecl = EnumSet.of(final_, ident, class_, lbrace, semicolon, eof);
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
  }

  private void Program() {
    check(program);
    check(ident);

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

    check(lbrace);
    while (!followSymbolMeth.contains(sym)) {
      MethodDecl();
    }
    check(rbrace);
  }

  private void MethodDecl() {
    switch (sym) {
      case void_ -> scan();
      case ident -> Type();
      default -> {
        error(INVALID_METH_DECL);
        recoverMethodDecl();
      }
    }


    check(ident);
    check(lpar);
    if (sym == ident) {
      FormPars();
    }

    check(rpar);
    while (sym == ident) {
      VarDecl();
    }
    Block();
  }

  void Type() {
    check(ident);
    if (sym == lbrack) {
      scan();
      check(rbrack);
    }
  }

  private void ClassDecl() {
    check(class_);
    check(ident);
    check(lbrace);
    while (sym == ident) {
      VarDecl();
    }
    check(rbrace);
  }

  void ConstDecl() {
    check(final_);
    Type();
    check(ident);
    check(assign);
    switch (sym) {
      case number, charConst -> scan();
      default -> error(CONST_DECL);
    }
    check(semicolon);
  }

  private void VarDecl() {
    Type();
    check(ident);
    while (sym == comma) {
      scan();
      check(ident);
    }
    check(semicolon);
  }

  private void FormPars() {
    Type();
    check(ident);
    while (sym == comma) {
      scan();
      Type();
      check(ident);
    }
  }

  private void Block() {
    check(lbrace);
    while (!followSymbolBlock.contains(sym)) {
      Statement();
    }
    check(rbrace);
  }

  private void Assignop() {
    // This can also be a long switch with multiple cases
    // where we check for each assignop
    // this is just way shorter
    // I think performance wise it should be equal as we are using a Set

    if (firstAssingop.contains(sym)) {
      scan();
    } else {
      error(ASSIGN_OP);
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
        Designator();
        if (firstAssingop.contains(sym)) {
          Assignop();
          Expr();
        } else if (sym == lpar) {
          ActPars();
        } else if (sym == pplus || sym == mminus) {
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
          Expr();
        }
        check(semicolon);
      }
      case read -> {
        scan();
        check(lpar);
        Designator();
        check(rpar);
        check(semicolon);
      }
      case print -> {
        scan();
        check(lpar);
        Expr();
        if (sym == comma) {
          scan();
          check(number);
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

  private void Expr() {
    if (sym == minus) {
      scan();
    }
    Term();

    while (sym == plus || sym == minus) {
      Addop();
      Term();
    }
  }

  private void Term() {
    Factor();
    while (firstMulop.contains(sym) || sym == exp) {
      if (sym == exp) {
        scan();
        check(number);
      } else {
        Mulop();
        Factor();
      }
    }
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

  private void Factor() {
    switch (sym) {
      case ident -> {
        Designator();
        if (sym == lpar) {
          ActPars();
        }
      }
      case new_ -> {
        scan();
        check(ident);
        if (sym == lbrack) {
          scan();
          Expr();
          check(rbrack);
        }
      }
      case lpar -> {
        scan();
        Expr();
        check(rpar);
      }
      case number, charConst -> scan();
      default -> error(INVALID_FACT);
    }
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

  private void Designator() {
    check(ident);
    boolean isDesignator = true;
    while (isDesignator) {
      switch (sym) {
        case period -> {
          scan();
          check(ident);
        }
        case lbrack -> {
          scan();
          Expr();
          check(rbrack);
        }
        default -> isDesignator = false;
      }
    }
  }

  private void Addop() {
    switch (sym) {
      case plus, minus -> scan();
      default -> error(ADD_OP);
    }
  }

  private void Mulop() {
    switch (sym) {
      case times, slash, rem -> scan();
      default -> error(MUL_OP);
    }
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
    if (sym == semicolon) scan();
    errDist = 0;
  }

  private void recoverMethodDecl() {
    do {
      scan();
    } while (!recoverSymbolMeth.contains(sym));
    errDist = 0;
  }
}
