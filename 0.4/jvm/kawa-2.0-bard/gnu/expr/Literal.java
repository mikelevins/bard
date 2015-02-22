package gnu.expr;
import gnu.bytecode.*;

/** A Literal contains compile-time information about a constant. */

public class Literal
{
  Literal next;

  public Field field;

  Object value;

  /* If field is non-null, it is the number of the Field.
   * I.e. if the index is 10, the value of the Literal is the value of
   * the static Field named Lit10. */
  int index;

  public Type type;
  
  public int flags;

  /** Set at the beginning of the call to writeObject. */
  static final int WRITING = 1;

  /** Set at the end of the call to writeObject. */
  static final int WRITTEN = 2;

  static final int CYCLIC = 4;

  /** In pass 2, object has been at least allocated. */
  static final int EMITTED = 8;

  /** Values produced by calling writeObject on value. */
  Object[] argValues;
  /** Types produced by calling writeObject on value. */
  Type[] argTypes;

  public static final Literal nullLiteral
    = new Literal(null, Type.nullType);

  public final Object getValue() { return value; }

  void assign (LitTable litTable)
  {
    assign((String) null, litTable);
  }

  /** Assign a static Field to hold the value of this Literal.
   * This supports the same value being used multiple times or cyclically. */
  void assign (String name, LitTable litTable)
  {
    int flags = litTable.comp.immediate ? Access.STATIC|Access.PUBLIC
      : Access.STATIC|Access.FINAL;
    if (name == null)
      {
	index = litTable.literalsCount++;
	name = "Lit" + index;
      }
    else
      flags |= Access.PUBLIC;
    assign(litTable.mainClass.addField (name, type, flags), litTable);
  }

  void assign (Field field, LitTable litTable)
  {
    next = litTable.literalsChain;
    litTable.literalsChain = this;
    this.field = field;
  }

  /** Create a new Literal, where comp must be in immediate mode. */
  public Literal (Object value, LitTable litTable)
  {
    this (value, (String) null, litTable);
  }

  public Literal (Object value, String name, LitTable litTable)
  {
    this.value = value;
    litTable.literalTable.put (value, this);
    this.type = Type.make(value.getClass());
    assign(name, litTable);
  }

  /** Create a new Literal, for a value available from a static field.
  * The field must be static and already exist. */
  public Literal (Object value, Field field, LitTable litTable)
  {
    this.value = value;
    litTable.literalTable.put(value, this);
    this.field = field;
    this.type = field.getType();
    flags = WRITTEN|EMITTED;
  }

  public Literal (Object value, Type type, LitTable litTable)
  {
    this.value = value;
    litTable.literalTable.put (value, this);
    this.type = type;
  }

  Literal (Object value, Type type)
  {
    this.value = value;
    this.type = type;
  }
}

