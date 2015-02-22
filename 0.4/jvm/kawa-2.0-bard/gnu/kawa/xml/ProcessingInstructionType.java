// Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import java.io.*;
import gnu.mapping.Symbol;

public class ProcessingInstructionType extends NodeType
implements TypeValue, Externalizable
{
  String target;

  public static final ProcessingInstructionType piNodeTest
  = new ProcessingInstructionType(null);

  public ProcessingInstructionType(String target)
  {
    super(target == null ? "processing-instruction()"
          : "processing-instruction("+target+")");
    this.target = target;
  }

  public static ProcessingInstructionType getInstance (String target)
  {
    return target == null ? piNodeTest : new ProcessingInstructionType(target);
  }

  public Type getImplementationType()
  {
    return ClassType.make("gnu.kawa.xml.KProcessingInstruction");

  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitPushString(target);
    code.emitInvokeStatic(coerceMethod);
  }

  public Object coerceFromObject (Object obj)
  {
    return coerce(obj, target);
  }

  public boolean isInstancePos (AbstractSequence seq, int ipos)
  {
    int kind = seq.getNextKind(ipos);
    if (kind == Sequence.PROCESSING_INSTRUCTION_VALUE)
      return target == null || target.equals(seq.getNextTypeObject(ipos));
    if (kind == Sequence.OBJECT_VALUE)
      return isInstance(seq.getPosNext(ipos));
    return false;
  }

  public boolean isInstance (Object obj)
  {
    return coerceOrNull(obj, target) != null;
  }

  public static KProcessingInstruction coerceOrNull (Object obj, String target)
  {
    KProcessingInstruction pos
      = (KProcessingInstruction) NodeType.coerceOrNull(obj, PI_OK);
    return pos != null && (target == null || target.equals(pos.getTarget()))
      ? pos : null;
  }

  public static KProcessingInstruction coerce (Object obj, String target)
  {
    KProcessingInstruction pos = coerceOrNull(obj, target);
    if (pos == null)
      throw new ClassCastException();
    return pos;
  }

  protected void emitCoerceOrNullMethod(Variable incoming, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    code.emitPushString(target);
    code.emitInvokeStatic(coerceOrNullMethod);
  }

  public static final ClassType typeProcessingInstructionType
    = ClassType.make("gnu.kawa.xml.ProcessingInstructionType");
  static final Method coerceMethod
    = typeProcessingInstructionType.getDeclaredMethod("coerce", 2);
  static final Method coerceOrNullMethod
    = typeProcessingInstructionType.getDeclaredMethod("coerceOrNull", 2);

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(target);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    target = (String) in.readObject();
  }

  public String toString ()
  {
    return "ProcessingInstructionType " + (target == null ? "*" : target);
  }
}
