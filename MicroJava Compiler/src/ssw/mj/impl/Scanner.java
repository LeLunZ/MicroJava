package ssw.mj.impl;

import ssw.mj.Errors;
import ssw.mj.scanner.Token;

import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;

public class Scanner {

  // Scanner Skeleton - do not rename fields / methods !
  private static final char EOF = (char) -1;
  private static final char LF = '\n';
  /**
   * Mapping from keyword names to appropriate token codes.
   */
  private static final Map<String, Token.Kind> keywords;

  static {
    keywords = new HashMap<>();
    keywords.put(Token.Kind.class_.label(), Token.Kind.class_);
    keywords.put(Token.Kind.program.label(), Token.Kind.program);
    keywords.put(Token.Kind.print.label(), Token.Kind.print);
    keywords.put(Token.Kind.break_.label(), Token.Kind.break_);
    keywords.put(Token.Kind.final_.label(), Token.Kind.final_);
    keywords.put(Token.Kind.else_.label(), Token.Kind.else_);
    keywords.put(Token.Kind.if_.label(), Token.Kind.if_);
    keywords.put(Token.Kind.new_.label(), Token.Kind.new_);
    keywords.put(Token.Kind.read.label(), Token.Kind.read);
    keywords.put(Token.Kind.return_.label(), Token.Kind.return_);
    keywords.put(Token.Kind.void_.label(), Token.Kind.void_);
    keywords.put(Token.Kind.while_.label(), Token.Kind.while_);
  }

  /**
   * According errors object.
   */
  public final Errors errors;
  /**
   * Input data to read from.
   */
  private final Reader in;
  /**
   * Lookahead character. (= next (unhandled) character in the input stream)
   */
  private char ch;
  /**
   * Current line in input stream.
   */
  private int line;
  /**
   * Current column in input stream.
   */
  private int col;


  public Scanner(Reader r) {
    // store reader
    in = r;

    // initialize error handling support
    errors = new Errors();

    line = 1;
    col = 0;
    nextCh(); // read 1st char into ch, incr col to 1
  }

  /**
   * Adds error message to the list of errors.
   */
  public final void error(Token t, Errors.Message msg, Object... msgParams) {
    errors.error(t.line, t.col, msg, msgParams);

    // reset token content (consistent JUnit tests)
    t.numVal = 0;
    t.val = null;
  }

  /**
   * Returns next token. To be used by parser.
   */
  public Token next() {
    while (Character.isWhitespace(ch)) {
      nextCh();
    }
    Token t = new Token(Token.Kind.none, line, col);

    switch (ch) {
      case 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ->
              readName(t);
      case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> readNumber(t);
      case '\'' -> {
        t.kind = Token.Kind.charConst;
        readCharConst(t);
      }
      case ';' -> {
        t.kind = Token.Kind.semicolon;
        nextCh();
      }
      case ',' -> {
        t.kind = Token.Kind.comma;
        nextCh();
      }
      case '.' -> {
        t.kind = Token.Kind.period;
        nextCh();
      }
      case '/' -> {
        nextCh();
        if (ch == '*') {
          skipComment(t);
          t = next();
        } else if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.slashas;
        } else {
          t.kind = Token.Kind.slash;
        }
      }
      case '=' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.eql;
        } else {
          t.kind = Token.Kind.assign;
        }
      }
      case '!' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.neq;
        } else {
          error(t, Errors.Message.INVALID_CHAR, '!');
        }
      }
      case '<' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.leq;
        } else {
          t.kind = Token.Kind.lss;
        }
      }
      case '>' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.geq;
        } else {
          t.kind = Token.Kind.gtr;
        }
      }
      case '+' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.plusas;
        } else if (ch == '+') {
          nextCh();
          t.kind = Token.Kind.pplus;
        } else {
          t.kind = Token.Kind.plus;
        }
      }
      case '-' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.minusas;
        } else if (ch == '-') {
          nextCh();
          t.kind = Token.Kind.mminus;
        } else {
          t.kind = Token.Kind.minus;
        }
      }
      case '*' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.timesas;
        } else if (ch == '*') {
          nextCh();
          t.kind = Token.Kind.exp;
        } else {
          t.kind = Token.Kind.times;
        }
      }
      case '%' -> {
        nextCh();
        if (ch == '=') {
          nextCh();
          t.kind = Token.Kind.remas;
        } else {
          t.kind = Token.Kind.rem;
        }
      }
      case '&' -> {
        nextCh();
        if (ch == '&') {
          nextCh();
          t.kind = Token.Kind.and;
        } else {
          error(t, Errors.Message.INVALID_CHAR, '&');
        }
      }
      case '|' -> {
        nextCh();
        if (ch == '|') {
          nextCh();
          t.kind = Token.Kind.or;
        } else {
          error(t, Errors.Message.INVALID_CHAR, '|');
        }
      }
      case '{' -> {
        nextCh();
        t.kind = Token.Kind.lbrace;
      }
      case '}' -> {
        nextCh();
        t.kind = Token.Kind.rbrace;
      }
      case '(' -> {
        nextCh();
        t.kind = Token.Kind.lpar;
      }
      case ')' -> {
        nextCh();
        t.kind = Token.Kind.rpar;
      }
      case '[' -> {
        nextCh();
        t.kind = Token.Kind.lbrack;
      }
      case ']' -> {
        nextCh();
        t.kind = Token.Kind.rbrack;
      }
      case EOF -> {
        t.kind = Token.Kind.eof;
      }
      default -> {
        error(t, Errors.Message.INVALID_CHAR, ch);
        nextCh();
      }
    }

    return t;
  }

  /**
   * Reads next character from input stream into ch. Keeps pos, line and col
   * in sync with reading position.
   */
  private void nextCh() {
    try {
      ch = (char) in.read();

      col++;
      if (ch == EOF) {
        return;
      }

      if (ch == LF) {
        line++;
        col = 0;
      }
    } catch (IOException e) {
      ch = EOF;
    }
  }

  public void skipComment(Token t) {
    nextCh();
    int nested = 1;

    while (nested > 0) {
      if (ch == EOF) {
        error(t, Errors.Message.EOF_IN_COMMENT);
        return;
      } else if (ch == '/') {
        nextCh();

        if (ch == '*') {
          nextCh();
          nested++;
        }
      } else if (ch == '*') {
        nextCh();

        if (ch == '/') {
          nextCh();
          nested--;
        }
      } else {
        nextCh();
      }
    }
  }

  public void readName(Token t) {
    StringBuilder sb = new StringBuilder();

    while (isLetter(ch) || isDigit(ch) || ch == '_') {
      sb.append(ch);
      nextCh();
    }

    t.val = sb.toString();
    t.kind = keywords.getOrDefault(t.val, Token.Kind.ident);
  }

  public void readNumber(Token t) {
    StringBuilder sb = new StringBuilder();

    while (isDigit(ch)) {
      sb.append(ch);
      nextCh();
    }

    t.val = sb.toString();
    t.kind = Token.Kind.number;

    try {
      t.numVal = Integer.parseInt(sb.toString());
    } catch (NumberFormatException e) {
      error(t, Errors.Message.BIG_NUM, t.val);
    }
  }

  public void readEscapedChar(Token t) {
    nextCh();

    switch (ch) {
      case 'n' -> {
        t.numVal = '\n';
        t.val = "\n";
      }
      case 'r' -> {
        t.numVal = '\r';
        t.val = "\r";
      }
      case '\'' -> {
        t.numVal = '\'';
        t.val = "'";
      }
      case '\\' -> {
        t.numVal = '\\';
        t.val = "\\";
      }
      default -> error(t, Errors.Message.UNDEFINED_ESCAPE, ch);
    }
  }

  public void readCharConst(Token t) {
    nextCh();

    switch (ch) {
      case EOF -> {
        error(t, Errors.Message.EOF_IN_CHAR);
        return;
      }
      case '\'' -> error(t, Errors.Message.EMPTY_CHARCONST);
      case '\\' -> {
        readEscapedChar(t);
        nextCh();

        if (ch != '\'') {
          error(t, Errors.Message.MISSING_QUOTE);
          return;
        }
      }
      case '\r', '\n' -> {
        error(t, Errors.Message.ILLEGAL_LINE_END);
      }
      default -> {
        t.numVal = ch;
        t.val = Character.toString(ch);
        nextCh();

        if (ch != '\'') {
          error(t, Errors.Message.MISSING_QUOTE);
          return;
        }
      }
    }
    nextCh();
  }

  private boolean isLetter(char c) {
    return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z';
  }

  private boolean isDigit(char c) {
    return '0' <= c && c <= '9';
  }

  // ================================================
  // ================================================
}
