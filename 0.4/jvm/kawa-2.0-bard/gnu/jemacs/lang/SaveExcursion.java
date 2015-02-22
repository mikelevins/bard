package gnu.jemacs.lang;
import kawa.lang.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.jemacs.buffer.*;
import gnu.kawa.reflect.Invoke;

public class SaveExcursion extends Syntax
{
  boolean bufferOnly;

  public static ClassType typeSaveExcursion
    = ClassType.make("gnu.jemacs.lang.SaveExcursion");
  public static ClassType typeBuffer
    = ClassType.make("gnu.jemacs.buffer.Buffer");

  public SaveExcursion(boolean bufferOnly)
  {
    this.bufferOnly = bufferOnly;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    tr.letStart();
    Declaration savedBuffer =
      tr.letVariable(null, typeBuffer,
                     Invoke.makeInvokeStatic(typeBuffer, "getCurrent"));
    tr.letEnter();
    if (bufferOnly)
      {
        Expression body = tr.rewrite_body(obj);
        Expression finalizer =
          Invoke.makeInvokeStatic(typeBuffer, "setBuffer",
                                  new ReferenceExp(savedBuffer));
        return tr.letDone(new TryExp(body, finalizer));
      }
    else
      {
        tr.letStart();
	Declaration savedPointMark =
          tr.letVariable(null, Type.longType,
                         Invoke.makeInvokeStatic(typeSaveExcursion, "savePointMark",
                                                 new ReferenceExp(savedBuffer)));
        tr.letEnter();
        Expression body = tr.rewrite_body(obj);
        Expression finalizer;
	finalizer = Invoke.makeInvokeStatic(typeSaveExcursion,
					   "restoreBufferPointMark",
                                            new ReferenceExp(savedBuffer),
                                            new ReferenceExp(savedPointMark));
        return tr.letDone(tr.letDone(new TryExp(body, finalizer)));
      }
  }

  /** Save point and (in the future) mark of a buffer.
   * Returns a pair (packed in a long) of buffer posistions. */
  public static long savePointMark(Buffer buffer)
  {
    return buffer.savePointMark();
  }

  public static void restoreBufferPointMark(Buffer buffer, long pointMark)
  {
    Buffer.setCurrent(buffer);
    buffer.restorePointMark(pointMark);
  }
}
