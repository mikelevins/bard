// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import java.io.*;
import gnu.mapping.Symbol;

/** Matches an attribute name pattern.
 * FIXME:  ElementType and AttributeType should both inherit
 * from a common NamedNodeType class. */

public class AttributeType extends NodeType
implements TypeValue, Externalizable, AttributePredicate
{
  Symbol qname;

  public static AttributeType make (String namespaceURI, String localName)
  {
    Symbol qname;
    if (namespaceURI != null)
      qname = Symbol.make(namespaceURI, localName);
    else if (localName == ElementType.MATCH_ANY_LOCALNAME)
      qname = ElementType.MATCH_ANY_QNAME;
    else
      qname = Symbol.makeUninterned(localName, null);
    return new AttributeType(qname);
  }

  public static AttributeType make (Symbol qname)
  {
    return new AttributeType(qname);
  }

  public AttributeType(Symbol qname)
  {
    this(null, qname);
  }

  public AttributeType(String name, Symbol qname)
  {
    super(name != null && name.length() > 0 ? name
	    : "ATTRIBUTE "+qname+" (*)");
    this.qname = qname;
  }

  public Type getImplementationType()
  {
    return ClassType.make("gnu.kawa.xml.KAttr");
  }

  public final String getNamespaceURI () { return qname.getNamespaceURI(); }
  public final String getLocalName () { return qname.getLocalName(); }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitPushString(qname.getNamespaceURI());
    code.emitPushString(qname.getLocalName());
    code.emitInvokeStatic(coerceMethod);
  }

  public Object coerceFromObject (Object obj)
  {
    return coerce(obj, qname.getNamespaceURI(), qname.getLocalName());
  }

  public boolean isInstancePos (AbstractSequence seq, int ipos)
  {
    int kind = seq.getNextKind(ipos);
    if (kind == Sequence.ATTRIBUTE_VALUE)
      return isInstance(seq, ipos, seq.getNextTypeObject(ipos));
    if (kind == Sequence.OBJECT_VALUE)
      return isInstance(seq.getPosNext(ipos));
    return false;
  }

  public boolean isInstance(AbstractSequence seq, int ipos, Object attrType)
  {
    String namespaceURI = qname.getNamespaceURI();
    String localName = qname.getLocalName();
    String curNamespaceURI;
    String curLocalName;
    if (attrType instanceof Symbol)
      {
	Symbol qname = (Symbol) attrType;
	curNamespaceURI = qname.getNamespaceURI();
	curLocalName = qname.getLocalName();
      }
    /* #ifdef JAXP-1.3 */
    else if (attrType instanceof javax.xml.namespace.QName)
      {
        javax.xml.namespace.QName qtype
          = (javax.xml.namespace.QName) attrType;
        curNamespaceURI = qtype.getNamespaceURI();
        curLocalName = qtype.getLocalPart();
      }
    /* #endif */
    else
      {
	curNamespaceURI = "";
	curLocalName = attrType.toString().intern();  // FIXME
      }
    if (localName != null && localName.length() == 0)
      localName = null;
    return ((localName == curLocalName || localName == null)
	    && (namespaceURI == curNamespaceURI || namespaceURI == null));
  }

  public boolean isInstance (Object obj)
  {
    return  coerceOrNull(obj, qname.getNamespaceURI(),qname.getLocalName())
      != null;
  }

  public static KAttr coerceOrNull (Object obj,
				    String namespaceURI, String localName)
  {
    KNode pos = NodeType.coerceOrNull(obj, ATTRIBUTE_OK);
    if (pos == null)
      return null;
    if (localName != null && localName.length() == 0)
      localName = null;
    Object curName = pos.getNextTypeObject();
    String curNamespaceURI;
    String curLocalName;
    if (curName instanceof Symbol)
      {
	Symbol qname = (Symbol) curName;
	curNamespaceURI = qname.getNamespaceURI();
	curLocalName = qname.getLocalName();
      }
    /* #ifdef JAXP-1.3 */
    else if (curName instanceof javax.xml.namespace.QName)
      {
        javax.xml.namespace.QName qtype
          = (javax.xml.namespace.QName) curName;
        curNamespaceURI = qtype.getNamespaceURI();
        curLocalName = qtype.getLocalPart();
      }
    /* #endif */
    else
      {
	curNamespaceURI = "";
	curLocalName = curName.toString().intern();  // FIXME
      }
    if ((localName == curLocalName || localName == null)
	&& (namespaceURI == curNamespaceURI || namespaceURI == null))
      return (KAttr) pos;
    return null;
  }

  public static SeqPosition coerce (Object obj,
				    String namespaceURI, String localName)
  {
    SeqPosition pos = coerceOrNull(obj, namespaceURI, localName);
    if (pos == null)
      throw new ClassCastException();
    return pos;
  }

  protected void emitCoerceOrNullMethod(Variable incoming, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    code.emitPushString(qname.getNamespaceURI());
    code.emitPushString(qname.getLocalName());
    code.emitInvokeStatic(coerceOrNullMethod);
  }

  public static final ClassType typeAttributeType
    = ClassType.make("gnu.kawa.xml.AttributeType");
  static final Method coerceMethod
    = typeAttributeType.getDeclaredMethod("coerce", 3);
  static final Method coerceOrNullMethod
    = typeAttributeType.getDeclaredMethod("coerceOrNull", 3);

  public void writeExternal(ObjectOutput out) throws IOException
  {
    String name = getName();
    out.writeUTF(name == null ? "" : name);
    out.writeObject(qname);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    String name = in.readUTF();
    if (name.length() > 0)
      setName(name);
    qname = (Symbol) in.readObject();
  }

  public String toString ()
  {
    return "AttributeType " + qname;
  }
}
