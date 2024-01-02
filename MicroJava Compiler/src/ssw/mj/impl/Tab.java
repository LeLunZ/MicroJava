package ssw.mj.impl;

import ssw.mj.symtab.Obj;
import ssw.mj.symtab.Scope;
import ssw.mj.symtab.Struct;

import static ssw.mj.Errors.Message.*;

public final class Tab {

  // Universe
  public static final Struct noType = new Struct(Struct.Kind.None);
  public static final Struct intType = new Struct(Struct.Kind.Int);
  public static final Struct charType = new Struct(Struct.Kind.Char);
  public static final Struct nullType = new Struct(Struct.Kind.Class);

  public final Obj noObj, chrObj, ordObj, lenObj;

  /**
   * Only used for reporting errors.
   */
  private final Parser parser;
  /**
   * The current top scope.
   */
  public Scope curScope;
  // First scope opening (universe) will increase this to -1
  /**
   * Nesting level of current scope.
   */
  private int curLevel = -2;

  public Tab(Parser p) {
    parser = p;

    curScope = new Scope(null);

    noObj = new Obj(Obj.Kind.Var, "$none", noType);

    // create predefined types
    insert(Obj.Kind.Type, "int", intType);
    insert(Obj.Kind.Type, "char", charType);
    insert(Obj.Kind.Con, "null", nullType);

    // predefined methods
    chrObj = insert(Obj.Kind.Meth, "chr", charType);
    openScope();
    insert(Obj.Kind.Var, "i", intType);
    chrObj.nPars = 1;
    chrObj.locals = curScope.locals();
    closeScope();

    ordObj = insert(Obj.Kind.Meth, "ord", intType);
    openScope();
    insert(Obj.Kind.Var, "ch", charType);
    ordObj.nPars = 1;
    ordObj.locals = curScope.locals();
    closeScope();

    lenObj = insert(Obj.Kind.Meth, "len", intType);
    openScope();
    insert(Obj.Kind.Var, "arr", new Struct(noType));
    lenObj.nPars = 1;
    lenObj.locals = curScope.locals();
    closeScope();

    curLevel = -1;
  }


  public void openScope() {
    curScope = new Scope(curScope);
    curLevel++;
  }

  public void closeScope() {
    curScope = curScope.outer();
    curLevel--;
  }

  public Obj insert(Obj.Kind kind, String name, Struct type) {
    if (curScope.findLocal(name) != null) {
      parser.error(DECL_NAME, name);
    }

    final Obj obj = new Obj(kind, name, type);
    if (kind == Obj.Kind.Var) {
      obj.adr = curScope.nVars();
      obj.level = curLevel;
    }

    curScope.insert(obj);
    return obj;
  }

  /**
   * Retrieves the object with <code>name</code> from the innermost scope.
   */
  public Obj find(String name) {
    final Obj foundObj = curScope.findGlobal(name);
    if (foundObj == null) {
      parser.error(NOT_FOUND, name);
      return noObj;
    } else {
      return foundObj;
    }
  }

  /**
   * Retrieves the field <code>name</code> from the fields of
   * <code>type</code>.
   */
  public Obj findField(String name, Struct type) {
    if (type == null || type.kind != Struct.Kind.Class) {
      parser.error(NO_FIELD, name);
      return noObj;
    }

    Obj foundField = type.fields.get(name);

    if (foundField == null) {
      parser.error(NO_FIELD, name);
      return noObj;
    } else {
      return foundField;
    }
  }

  // ===============================================
  // ===============================================
}
