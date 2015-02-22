package gnu.expr;
import gnu.bytecode.*;

/** A piece of code that needs to be added to <clinit>, <init>, or whatever. */

public abstract class Initializer
{
  Initializer next;

  /** If non-null:  The Field that is being initialized. */
  public Field field;

  public abstract void emit(Compilation comp);

  public static Initializer reverse(Initializer list)
  {
    // Algorithm takes from gcc's tree.c.
    Initializer prev = null;
    while (list != null)
      {
        Initializer next = list.next;
        list.next = prev;
        prev = list;
        list = next;
      }
    return prev;
  }

  public void reportError (String message, Compilation comp)
  {
    comp.error('e', message+"field "+field);
  }
}
