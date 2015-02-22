// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
/* #ifdef use:org.w3c.dom.Node */
import org.w3c.dom.*;
/* #endif */

public class KElement extends KNode
  /* #ifdef use:org.w3c.dom.Node */
  implements org.w3c.dom.Element
  /* #endif */
{
  public KElement (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }
  
  /* #ifdef use:org.w3c.dom.Node */
  public short getNodeType () { return Node.ELEMENT_NODE; }
  /* #endif */

  public String getTagName ()
  {
    return sequence.getNextTypeName(ipos);
  }

  public String getNodeValue()
  {
    return null;
  }

  public boolean hasAttributes ()
  {
    return ((NodeTree) sequence).posHasAttributes(ipos);
  }

  public String getAttribute (String name)
  {
    if (name == null)
      name = "";
    NodeTree nodes = (NodeTree) sequence;
    int attr = nodes.getAttribute(ipos, null, name);
    if (attr == 0)
      return "";
    else
      return KNode.getNodeValue(nodes, attr);
  }

  /* #ifdef use:org.w3c.dom.Node */
  /** Not implemented. */
  public void setAttribute (String name, String value)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setAttribute not supported");
  }

  /** Not implemented. */
  public void setIdAttribute (String name, boolean isId)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setIdAttribute not supported");
  }

  /** Not implemented. */
  public void setIdAttributeNS (String namespaceURI, String localName,
                                boolean isId)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setIdAttributeNS not supported");
  }

  /** Not implemented. */
  public void setIdAttributeNode (Attr idAttr, boolean isId)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setIdAttributeNode not supported");
  }

  /** Not implemented. */
  public void removeAttribute (String name)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "removeAttribute not supported");
  }
  /* #endif */

  /* #ifdef JAVA5 */
  public KAttr
  /* #else */
  /* #ifdef use:org.w3c.dom.Node */
  // public Attr
  /* #else */
  // public KAttr
  /* #endif */
  /* #endif */
  getAttributeNode (String name)
  {
    if (name == null)
      name = "";
    NodeTree nodes = (NodeTree) sequence;
    int attr = nodes.getAttribute(ipos, null, name);
    if (attr == 0)
      return null;
    else
      return new KAttr(nodes, attr);
  }

  /* #ifdef use:org.w3c.dom.Node */
  /** Not implemented. */
   public Attr setAttributeNode (Attr newAttr)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setAttributeNode not supported");
  }

  /** Not implemented. */
   public Attr removeAttributeNode (Attr oldAttr)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "removeAttributeNode not supported");
  }
  /* #endif */

  public String getAttributeNS (String namespaceURI, String localName)
  {
    if (namespaceURI == null)
      namespaceURI = "";
    if (localName == null)
      localName = "";
    NodeTree nodes = (NodeTree) sequence;
    int attr = nodes.getAttribute(ipos, namespaceURI, localName);
    if (attr == 0)
      return "";
    else
      return getNodeValue(nodes, attr);
  }

  /* #ifdef use:org.w3c.dom.Node */
  /** Not implemented. */
  public void setAttributeNS (String namespaceURI, String qualifiedName, 
                              String value) throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setAttributeNS not supported");
  }
  /* #endif */

  /* #ifdef use:org.w3c.dom.Node */
  /** Not implemented. */
  public void removeAttributeNS (String namespaceURI, String localName)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "removeAttributeNS not supported");
  }
  /* #endif */
 
  /* #ifdef JAVA5 */
  public KAttr
  /* #else */
  /* #ifdef use:org.w3c.dom.Node */
  // public Attr
  /* #else */
  // public KAttr
  /* #endif */
  /* #endif */
  getAttributeNodeNS(String namespaceURI, String localName)
  {
    if (namespaceURI == null)
      namespaceURI = "";
    if (localName == null)
      localName = "";
    NodeTree nodes = (NodeTree) sequence;
    int attr = nodes.getAttribute(ipos, namespaceURI, localName);
    if (attr == 0)
      return null;
    else
      return new KAttr(nodes, attr);
  }

  /** Not implemented. */
  /* #ifdef use:org.w3c.dom.Node */
  public Attr setAttributeNodeNS (Attr newAttr)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setAttributeNodeNS not supported");
  }
  /* #endif */

  // Not implemented yet.
  /* #ifdef use:org.w3c.dom.Node */
  public NodeList getElementsByTagNameNS(String namespaceURI, String localName)
  {
    throw new UnsupportedOperationException("getElementsByTagNameNS not implemented yet");
  }
  /* #endif */

  /** Not implemented yet. */
  public boolean hasAttribute (String name)
  {
    int attr = ((NodeTree) sequence).getAttribute(ipos, null, name == null ? "" : name);
    return attr != 0;
  }

  public boolean hasAttributeNS (String namespaceURI, String localName)
  {
    if (namespaceURI == null)
      namespaceURI = "";
    if (localName == null)
      localName = "";
    int attr = ((NodeTree) sequence).getAttribute(ipos, namespaceURI, localName);
    return attr != 0;
  }

  /* #ifdef JAXP-1.3 */
  public TypeInfo getSchemaTypeInfo ()
  {
    return null;
  }
  /* #endif JAXP-1.3 */
}
