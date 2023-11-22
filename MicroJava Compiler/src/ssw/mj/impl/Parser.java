package ssw.mj.impl;

import ssw.mj.Errors;
import ssw.mj.Errors.Message;
import ssw.mj.scanner.Token;

import java.util.EnumSet;

import static ssw.mj.scanner.Token.Kind.*;
import static ssw.mj.Errors.Message.*;

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
      error(TOKEN_EXPECTED, expected);
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
    check(eof);
  }

  private void Program() {
    check(program);
    check(ident);

    while (sym == final_ || sym == ident || sym == class_) {
      switch (sym) {
        case class_ -> ClassDecl();
        case ident -> VarDecl();
        case final_ -> ConstDecl();
      }
    }

    check(lbrace);
    while (sym == ident || sym == void_) {
      MethodDecl();
    }
    check(rbrace);
  }

  private void MethodDecl() {
    switch (sym) {
      case void_ -> scan();
      case ident -> Type();
      default -> error(INVALID_METH_DECL);
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
    while (firstStatement.contains(sym)) {
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
        } else {
          switch (sym) {
            case lpar -> ActPars();
            case pplus, mminus -> scan();
            default -> error(DESIGN_FOLLOW);
          }
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
      default -> error(INVALID_STAT);
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
    while (sym == times || sym == slash || sym == rem || sym == exp) {
      switch (sym) {
        case times, slash, rem -> {
          Mulop();
          Factor();
        }
        case exp -> {
          scan();
          check(number);
        }
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

  // ------------------------------------

  // TODO Exercise 3: Error recovery methods: recoverDecl, recoverMethodDecl and recoverStat

  // ====================================
  // ====================================
}
