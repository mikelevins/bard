// Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import java.io.*;
import gnu.mapping.*;

public class ElementType extends NodeType
implements TypeValue, Externalizable, ElementPredicate
{
  public static final String MATCH_ANY_LOCALNAME = "";
  public static final Symbol MATCH_ANY_QNAME
  = Symbol.makeUninterned(MATCH_ANY_LOCALNAME, null);

  Symbol qname;

    public static final ElementType anyElement = make((String) null, (String) null);

  /** An element type for match by name.
   * @param localName if null matches any local name; otherwise must
   *  be intered, and matches by identity.
   * @param namespaceURI full name of namespace, or null for any namespace. */
  public static ElementType make (String namespaceURI, String localName)
  {
    Symbol qname;
    if (namespaceURI != null)
      qname = Symbol.make(namespaceURI, localName);
    else if (localName == MATCH_ANY_LOCALNAME)
      qname = MATCH_ANY_QNAME;
    else
      qname = Symbol.makeUninterned(localName, null);
    return new ElementType(qname);
  }

  public static ElementType make (Symbol qname)
  {
    return new ElementType(qname);
  }

  public ElementType(Symbol qname)
  {
    this(null, qname);
  }

  public ElementType(String name, Symbol qname)
  {
    super(name != null && name.length() > 0 ? name
	    : "ELEMENT "+qname+" (*)");
    this.qname = qname;
  }

  public Type getImplementationType()
  {
    return ClassType.make("gnu.kawa.xml.KElement");
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
    if (kind == Sequence.ELEMENT_VALUE)
      return isInstance(seq, ipos, seq.getNextTypeObject(ipos));
    if (kind == Sequence.OBJECT_VALUE)
      return isInstance(seq.getPosNext(ipos));
    return false;
  }

  public boolean isInstance(AbstractSequence seq, int ipos, Object elementType)
  {
    String namespaceURI = qname.getNamespaceURI();
    String localName = qname.getLocalName();
    String curNamespaceURI;
    String curLocalName;
    if (elementType instanceof Symbol)
      {
	Symbol qname = (Symbol) elementType;
	curNamespaceURI = qname.getNamespaceURI();
	curLocalName = qname.getLocalName();
      }
    /* #ifdef JAXP-1.3 */
    else if (elementType instanceof javax.xml.namespace.QName)
    {
      javax.xml.namespace.QName qtype
        = (javax.xml.namespace.QName) elementType;
      curNamespaceURI = qtype.getNamespaceURI();
      curLocalName = qtype.getLocalPart();
    }
    /* #endif */
    else
      {
	curNamespaceURI = "";
	curLocalName = elementType.toString().intern();  // FIXME
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

  public static KElement coerceOrNull (Object obj,
				       String namespaceURI, String localName)
  {
    KElement pos = (KElement) NodeType.coerceOrNull(obj, ELEMENT_OK);
    if (pos == null)
      return null;
    if (localName != null && localName.length() == 0)
      localName = null;
    //if (namespaceURI != null && namespaceURI.length() == 0)
    // namespaceURI = null;
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
      return pos;
    return null;
  }

  public static KElement coerce (Object obj,
				 String namespaceURI, String localName)
  {
    KElement pos = coerceOrNull(obj, namespaceURI, localName);
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

  NamespaceBinding namespaceNodes;
  public NamespaceBinding getNamespaceNodes ()
  {
    return namespaceNodes;
  }

  public void setNamespaceNodes (NamespaceBinding bindings)
  {
    namespaceNodes = bindings;
  }

  public Procedure getConstructor ()
  {
    gnu.kawa.xml.MakeElement element = new gnu.kawa.xml.MakeElement();
    element.tag = qname;
    element.setHandlingKeywordParameters(true);
    if (namespaceNodes != null)
      element.setNamespaceNodes(namespaceNodes);
    return element;
  }

  public static final ClassType typeElementType
    = ClassType.make("gnu.kawa.xml.ElementType");
  static final Method coerceMethod
    = typeElementType.getDeclaredMethod("coerce", 3);
  static final Method coerceOrNullMethod
    = typeElementType.getDeclaredMethod("coerceOrNull", 3);

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
    return "ElementType " + qname;
  }
}
