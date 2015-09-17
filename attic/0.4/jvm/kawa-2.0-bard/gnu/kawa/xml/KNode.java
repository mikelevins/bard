// Copyright (c) 2004, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.Path;
import gnu.lists.*;
import gnu.xml.*;
/* #ifdef use:org.w3c.dom.Node */
import org.w3c.dom.*;
/* #endif */
import gnu.mapping.*;

public abstract class KNode extends SeqPosition<Object,NodeTree>
  implements
  /* #ifdef use:org.w3c.dom.Node */
  org.w3c.dom.Node,
  /* #endif */
  Consumable
{

  public KNode (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public static Object atomicValue (Object value)
  {
    if (value instanceof KNode)
      {
        KNode node = (KNode) value;
        return ((NodeTree) node.sequence).typedValue(node.ipos);
      }
    return value;
  }

  /** Convert value to a KNode, returning null if it isn't a node. */
  public static KNode coerce (Object value)
  {
    if (value instanceof KNode)
      return (KNode) value;
    if (value instanceof NodeTree)
      {
	NodeTree ntree = (NodeTree) value;
	return make(ntree, ntree.startPos());
      }
    if (value instanceof SeqPosition
	&& ! (value instanceof TreePosition))
      {
	SeqPosition seqp = (SeqPosition) value;
	if (seqp.sequence instanceof NodeTree)
	  return make((NodeTree) seqp.sequence, seqp.ipos);
      }
    return null;
  }

  public static KNode make(NodeTree seq, int ipos)
  {
    int index = seq.posToDataIndex(ipos);
    while (index < seq.data.length
           && seq.data[index] == TreeList.BEGIN_ENTITY)
      {
        index += TreeList.BEGIN_ENTITY_SIZE;
        if (index == seq.gapStart)
          index = seq.gapEnd;
      }
    ipos = index << 1;
    int kind = seq.getNextKindI(index);
    switch (kind)
      {
      case Sequence.ELEMENT_VALUE:
	return new KElement(seq, ipos);
      case Sequence.ATTRIBUTE_VALUE:
	return new KAttr(seq, ipos);
      case Sequence.DOCUMENT_VALUE:
	return new KDocument(seq, ipos);
      case Sequence.CDATA_VALUE:
	return new KCDATASection(seq, ipos);
      case Sequence.COMMENT_VALUE:
	return new KComment(seq, ipos);
      case Sequence.PROCESSING_INSTRUCTION_VALUE:
	return new KProcessingInstruction(seq, ipos);
      case Sequence.EOF_VALUE:
	if (! seq.isEmpty())
	  return null;
	// .. else fall through to create an empty text node.
      default:
	return new KText(seq, ipos);
      }
  }

  /* #ifdef JAVA5 */
  public KNode copy ()
  {
    return make((NodeTree) sequence, sequence.copyPos(getPos()));
  }
  /* #else */
  // public SeqPosition copy ()
  // {
  //   return make((NodeTree) sequence, sequence.copyPos(getPos()));
  // }
  /* #endif */
 
  public static KNode make(NodeTree seq)
  {
    return make(seq, 0);
  }

  public boolean isSupported(String feature, String version)
  {
    return false;
  }

  /* #ifdef use:org.w3c.dom.Node */
  public abstract short getNodeType ();
  /* #endif */

  public String getNodeName ()
  {
    return sequence.getNextTypeName(ipos);
  }

  /** The Data Model's node-name accessor.
   * Return the node's name as a SSymbol (QName) or null if there is none.
   */
  public Symbol getNodeSymbol ()
  {
    Object type = ((NodeTree) sequence).getNextTypeObject(ipos);
    if (type == null)
      return null;
    if (type instanceof Symbol)
      return (Symbol) type;
    return Namespace.EmptyNamespace.getSymbol(type.toString().intern());
  }

  /** Get the raw "type object" of a node.
   */
  public Object getNodeNameObject ()
  {
    return ((NodeTree) sequence).getNextTypeObject(ipos);
  }

  public String getNamespaceURI ()
  {
    return ((NodeTree) sequence).posNamespaceURI(ipos);
  }

  public String getPrefix ()
  {
    return ((NodeTree) sequence).posPrefix(ipos);
  }

  public String getLocalName ()
  {
    return ((NodeTree) sequence).posLocalName(ipos);
  }

  public static String getNodeValue (NodeTree seq, int ipos)
  {
    StringBuffer sbuf = new StringBuffer();
    getNodeValue(seq, ipos, sbuf);
    return sbuf.toString();
  }

  public static void getNodeValue (NodeTree seq, int ipos, StringBuffer sbuf)
  {
    seq.stringValue(seq.posToDataIndex(ipos), sbuf);
  }

  public String getNodeValue()
  {
    StringBuffer sbuf = new StringBuffer();
    getNodeValue(sbuf);
    return sbuf.toString();
  }

  public void getNodeValue (StringBuffer sbuf)
  {
    getNodeValue((NodeTree) sequence, ipos, sbuf);
  }

  public boolean hasChildNodes()
  {
    return ((NodeTree) sequence).posFirstChild(ipos) >= 0;
  }

  public String getTextContent ()
  {
    StringBuffer sbuf = new StringBuffer();
    getTextContent(sbuf);
    return sbuf.toString();
  }

  protected void getTextContent (StringBuffer sbuf)
  {
    // What is the difference between getTextContent and getNodeValue?  FIXME.
    getNodeValue(sbuf);
  }

  /* #ifdef use:org.w3c.dom.Node */
  public Node getParentNode()
  {
    int parent = sequence.parentPos(ipos);
    if (parent == -1)
      return null;
    return make((NodeTree) sequence, parent);
  }

  public Node getPreviousSibling ()
  {
    int parent = sequence.parentPos(ipos);
    if (parent == -1)
      parent = 0;
    int index = ((NodeTree) sequence).posToDataIndex(ipos);
    int child = sequence.firstChildPos(parent);
    int previous = 0;
    for (;;)
      {
        previous = child;
        child = sequence.nextPos(child);
        if (child == 0)
          break;
        if (((NodeTree) sequence).posToDataIndex(child) == index)
          break;
      }
    return previous == 0 ? null
      : make((NodeTree) sequence, previous);
  }
  /* #endif */

  /* #ifdef use:org.w3c.dom.Node */
  public Node getNextSibling ()
  {
    int next = ((NodeTree) sequence).nextPos(ipos);
    return next == 0 ? null
      : make((NodeTree) sequence, next);
  }

  public Node getFirstChild()
  {
    int child = ((NodeTree) sequence).posFirstChild(ipos);
    return make((NodeTree) sequence, child);
  }

  public Node getLastChild()
  {
    int last = 0;
    int child = sequence.firstChildPos(ipos);
    while (child != 0)
      {
        last = child;
        child = sequence.nextPos(child);
      }
    return last == 0 ? null : make((NodeTree) sequence, last);
  }

  public NodeList getChildNodes ()
  {
    Nodes nodes = new SortedNodes();
    int child = sequence.firstChildPos(ipos);
    while (child != 0)
      {
        nodes.writePosition(sequence, child);
        child = sequence.nextPos(child);
      }
    return nodes;
  }

  /** Not implemented yet. */
  public NodeList getElementsByTagName(String tagname)
  {
    throw new UnsupportedOperationException("getElementsByTagName not implemented yet");
    /*
    Nodes nodes = new SortedNodes();
    int child = sequence.firstChildPos(ipos);
    while (child != 0)
      {
        if (matches)
          nodes.writePosition(sequence, child);
        child = sequence.nextPos(child);
      }
    return nodes;
    */
  }
  /* #endif */

  /* #ifdef use:org.w3c.dom.Node */
  /** Not implemented. */
  public void setNodeValue (String nodeValue)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setNodeValue not supported");
  }

  /** Not implemented. */
  public void setPrefix (String prefix)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setPrefix not supported");
  }

  /** Not implemented. */
   public Node insertBefore(Node newChild, Node refChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "insertBefore not supported");
  }

  /** Not implemented. */
   public Node replaceChild(Node newChild, Node oldChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "replaceChild not supported");
  }

  /** Not implemented. */
   public Node removeChild(Node oldChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "removeChild not supported");
  }

  /** Not implemented. */
   public Node appendChild(Node newChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "appendChild not supported");
  }

  /** Not implemented. */
   public void setTextContent (String textContent)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setTextContent not supported");
  }

  /** Only implemented if deep is true. */
  public Node cloneNode(boolean deep)
  {
    if (! deep)
      throw new UnsupportedOperationException("shallow cloneNode not implemented");
    NodeTree tree = new NodeTree();
    ((NodeTree) sequence).consumeNext(ipos, tree);
    return make(tree);
  }

  public org.w3c.dom.Document getOwnerDocument ()
  {
    int kind = sequence.getNextKind(ipos);
    if (kind == Sequence.DOCUMENT_VALUE)
      return new KDocument((NodeTree) sequence, 0);
    return null;
  }

  public NamedNodeMap getAttributes ()
  {
    throw new UnsupportedOperationException("getAttributes not implemented yet");
  }
  /* #endif */

  public void normalize ()
  {
  }

  public boolean hasAttributes ()
  {
    return false;
  }

  public boolean isDefaultNamespace (String namespaceURI)
  {
    return ((NodeTree) sequence).posIsDefaultNamespace(ipos, namespaceURI);
  }

  public String lookupNamespaceURI (String prefix)
  {
    return ((NodeTree) sequence).posLookupNamespaceURI(ipos, prefix);
  }

  public String lookupPrefix (String namespaceURI)
  {
    return ((NodeTree) sequence).posLookupPrefix(ipos, namespaceURI);
  }

  public String getBaseURI ()
  {
    Object uri = ((NodeTree) sequence).baseUriOfPos(ipos, true);
    return uri == null ? null : uri.toString();
  }

  public Path baseURI ()
  {
    return ((NodeTree) sequence).baseUriOfPos(ipos, true);
  }

  /* #ifdef use:org.w3c.dom.Node */
  public short compareDocumentPosition (Node other)
    throws DOMException
  {
    if (! (other instanceof KNode))
      throw new DOMException(DOMException.NOT_SUPPORTED_ERR,
                             "other Node is a "+other.getClass().getName());
    KNode n = (KNode) other;
    AbstractSequence nseq = n.sequence;
    return (short) (sequence == nseq ? nseq.compare(ipos, n.ipos)
                    : (int) sequence.stableCompare(nseq));
  }
    
  public boolean isSameNode (Node node)
  {
    if (! (node instanceof KNode))
      return false;
    KNode n = (KNode) node;
    if (sequence != n.sequence)
      return false;
    return sequence.equals(ipos, n.ipos);
  }

  public boolean isEqualNode (Node node)
  {
    throw new UnsupportedOperationException("getAttributesisEqualNode not implemented yet");
  }
  /* #endif */

  public String toString ()
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    XMLPrinter xp = new XMLPrinter(wr);
    ((NodeTree) sequence).consumeNext(ipos, xp);
    xp.close();
    wr.close();
    return wr.toString();
  }

  public Object getFeature (String feature, String version)
  {
    return null;
  }

  public void consume(Consumer out)
  {
    if (out instanceof PositionConsumer)
      ((PositionConsumer) out).writePosition(this);
    else
      ((NodeTree) sequence).consumeNext(ipos, out);
  }

  /* #ifdef JAXP-1.3 */
  public Object setUserData (String key, Object data, UserDataHandler handler)
  {
    throw new UnsupportedOperationException("setUserData not implemented yet");
  }

  public Object getUserData (String key)
  {
    return null;
  }
  /* #endif JAXP-1.3 */
}
