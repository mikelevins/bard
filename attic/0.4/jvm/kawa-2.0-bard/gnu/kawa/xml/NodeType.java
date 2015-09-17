// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.Procedure;
import gnu.lists.*;
import java.io.*;
import gnu.expr.*;
import gnu.xml.*;

/** A SeqPosition used to represent a node in (usually) a TreeList.
 * This is special in that the represented node is the current position
 * of the SeqPosition - but when passed to a method it is only valid
 * during that method.  After the method returns, the caller is free to
 * change the position, so if the node is saved in a data structure it
 * must be copied. */

public class NodeType extends ObjectType implements TypeValue, NodePredicate, Externalizable
{
  public static final int TEXT_OK = 1;
  public static final int ELEMENT_OK = 2;
  public static final int ATTRIBUTE_OK = 4;
  public static final int DOCUMENT_OK = 8;
  public static final int COMMENT_OK = 16;
  public static final int PI_OK = 32;
  int kinds = -1;

  public NodeType(String name, int kinds)
  {
    super(name);
    this.kinds = kinds;
  }

  public NodeType(String name)
  {
    this(name, -1);
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitPushInt(kinds);
    code.emitInvokeStatic(coerceMethod);
  }

  public Expression convertValue (Expression value)
  {
    ApplyExp aexp = new ApplyExp(coerceMethod, new Expression[] { value });
    aexp.setType(this);
    return aexp;
  }

  public Object coerceFromObject (Object obj)
  {
    return coerceForce(obj, kinds);
  }

  public Type getImplementationType()
  {
    return typeKNode;
  }

  public int compare(Type other)
  {
    return getImplementationType().compare(other);
  }

  public boolean isInstance (Object obj)
  { 
    if (obj instanceof KNode)
      {
	KNode pos = (KNode) obj;
	return isInstancePos(pos.sequence, pos.getPos());
      }
    return false;
  }

  public boolean isInstancePos(AbstractSequence seq, int ipos)
  {
    return isInstance(seq, ipos, kinds);
  }

  public static boolean isInstance(AbstractSequence seq, int ipos, int kinds)
  {
    int kind = seq.getNextKind(ipos);
    if (kinds < 0)
      return kind != Sequence.EOF_VALUE;
    else
      {
	switch (kind)
	  {
	  case Sequence.EOF_VALUE:
	    return false;
	  case Sequence.INT_U8_VALUE:
	  case Sequence.INT_S8_VALUE:
	  case Sequence.INT_U16_VALUE:
	  case Sequence.INT_S16_VALUE:
	  case Sequence.INT_U32_VALUE:
	  case Sequence.INT_S32_VALUE:
	  case Sequence.INT_U64_VALUE:
	  case Sequence.INT_S64_VALUE:
	  case Sequence.FLOAT_VALUE:
	  case Sequence.DOUBLE_VALUE:
	  case Sequence.BOOLEAN_VALUE:
	  case Sequence.TEXT_BYTE_VALUE:
	  case Sequence.CHAR_VALUE:
	  case Sequence.OBJECT_VALUE:
	    return (kinds & TEXT_OK) != 0;
	  case Sequence.ELEMENT_VALUE:
	    return (kinds & ELEMENT_OK) != 0;
	  case Sequence.ATTRIBUTE_VALUE:
	    return (kinds & ATTRIBUTE_OK) != 0;
	  case Sequence.DOCUMENT_VALUE:
	    return (kinds & DOCUMENT_OK) != 0;
	  case Sequence.COMMENT_VALUE:
	    return (kinds & COMMENT_OK) != 0;
	  case Sequence.PROCESSING_INSTRUCTION_VALUE:
	    return (kinds & PI_OK) != 0;
	  }
      }
    return true;
  }

  public static KNode coerceForce(Object obj, int kinds)
  {
    KNode pos = coerceOrNull(obj, kinds);
    if (pos == null)
      throw new ClassCastException("coerce from "+obj.getClass());
    return pos;
  }

  public static KNode coerceOrNull(Object obj, int kinds)
  {
    KNode pos;
    if (obj instanceof NodeTree)
      pos = KNode.make((NodeTree) obj);
    else if (obj instanceof KNode)
      pos = (KNode) obj;
    else
      return null;
    return isInstance(pos.sequence, pos.ipos, kinds) ? pos : null;
  }

  protected void emitCoerceOrNullMethod(Variable incoming,
					Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    code.emitPushInt(kinds);
    code.emitInvokeStatic(coerceOrNullMethod);
  }

  public void emitTestIf(Variable incoming, Declaration decl, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    emitCoerceOrNullMethod(incoming, comp);
    if (decl != null)
      {
	code.emitDup();
	decl.compileStore(comp);
      }
    code.emitIfNotNull();
  }

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	emitCoerceOrNullMethod(incoming, comp);
	CodeAttr code = comp.getCode();
	if (ctarget.trueBranchComesFirst)
	  code.emitGotoIfCompare1(ctarget.ifFalse, 198); // ifnull
	else
	  code.emitGotoIfCompare1(ctarget.ifTrue, 199); // ifnonnull
	ctarget.emitGotoFirstBranch(code);
      }
    else
      gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
  }

  public static final ClassType typeKNode = ClassType.make("gnu.kawa.xml.KNode");
  public static final ClassType typeNodeType = ClassType.make("gnu.kawa.xml.NodeType");
  public static final NodeType nodeType = new NodeType("gnu.kawa.xml.KNode");
  static final Method coerceMethod
    = typeNodeType.getDeclaredMethod("coerceForce", 2);
  static final Method coerceOrNullMethod
    = typeNodeType.getDeclaredMethod("coerceOrNull", 2);

  public Procedure getConstructor ()
  {
    return null;
  }

  public String toString ()
  {
    return "NodeType " + getName();
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    String name = getName();
    out.writeUTF(name == null ? "" : name);
    out.writeInt(kinds);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    String name = in.readUTF();
    if (name.length() > 0)
      setName(name);
    kinds = in.readInt();
  }

    /* #ifndef JAVA8 */
    // public String encodeType(Language language) { return null; }
    /* #endif */

  public static final NodeType documentNodeTest =
    new NodeType("document-node", DOCUMENT_OK);
  public static final NodeType textNodeTest =
    new NodeType("text", TEXT_OK);
  public static final NodeType commentNodeTest =
    new NodeType("comment", COMMENT_OK);
  public static final NodeType anyNodeTest =
    new NodeType("node");
}
