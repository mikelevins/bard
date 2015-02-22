// Copyright (c) 2004, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import gnu.lists.Sequence;
/* #ifdef use:org.w3c.dom.Node */
import org.w3c.dom.*;
/* #endif */

public class KDocument extends KNode
  /* #ifdef use:org.w3c.dom.Node */
  implements org.w3c.dom.Document
  /* #endif */
{
  public KDocument (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }
  
  public String getNodeName()
  {
    return "#document";
  }

  /* #ifdef use:org.w3c.dom.Node */
  public DOMImplementation getImplementation ()
  {
    throw new UnsupportedOperationException("getImplementation not implemented");
  }

  public DocumentType getDoctype ()
  {
    return null;
  }

  public Node getParentNode()
  {
    return null;
  }
  /* #endif */

  public KElement getDocumentElement ()
  {
    int child = ((NodeTree) sequence).posFirstChild(ipos);
    for (;;)
      {
        if (child == -1)
          return null;
        if (sequence.getNextKind(child) != Sequence.COMMENT_VALUE)
          break;
        child = sequence.nextPos(child);
      }
    return (KElement) make((NodeTree) sequence, child);
  }

  /* #ifdef use:org.w3c.dom.Node */
  public short getNodeType () { return Node.DOCUMENT_NODE; }
  /* #endif */

  public String getNodeValue()
  {
    return null;
  }

  public String getTextContent ()
  {
    return null;
  }

  protected void getTextContent (StringBuffer sbuf)
  {
    // Do nothing.
  }

  /* #ifdef use:org.w3c.dom.Node */
  /** Not implemented. */
   public Element createElement (String tagName)
  {
    throw new UnsupportedOperationException("createElement not implemented");
  }

  /** Not implemented. */
  public DocumentFragment createDocumentFragment ()
  {
    throw new UnsupportedOperationException("createDocumentFragment not implemented");
  }

  /** Not implemented. */
  public Text createTextNode (String data)
  {
    throw new UnsupportedOperationException("createTextNode not implemented");
  }

  /** Not implemented. */
  public Comment createComment (String data)
  {
    throw new UnsupportedOperationException("createComment not implemented");
  }

  /** Not implemented. */
  public CDATASection createCDATASection (String data)
  {
    throw new UnsupportedOperationException("createCDATASection not implemented");
  }

  /** Not implemented. */
  public ProcessingInstruction createProcessingInstruction (String target, 
                                                            String data)
  {
    throw new UnsupportedOperationException("createProcessingInstruction not implemented");
  }

  /** Not implemented. */
  public Attr createAttribute (String name)
  {
    throw new UnsupportedOperationException("createAttribute not implemented");
  }

  /** Not implemented. */
  public EntityReference createEntityReference (String name)
  {
    throw new UnsupportedOperationException("createEntityReference implemented");
  }

  /** Not implemented. */
  public Node importNode (Node importedNode, boolean deep)
  {
    throw new UnsupportedOperationException("importNode not implemented");
  }

  /** Not implemented. */
  public Element createElementNS (String namespaceURI, String qualifiedName)
  {
    throw new UnsupportedOperationException("createElementNS not implemented");

  }

  /** Not implemented. */
  public Attr createAttributeNS (String namespaceURI, String qualifiedName)
  {
    throw new UnsupportedOperationException("createAttributeNS not implemented");
  }

  /** Not implemented yet. */
  public NodeList getElementsByTagNameNS(String namespaceURI, String localName)
  {
    throw new UnsupportedOperationException("getElementsByTagNameNS not implemented yet");
  }

  public Element getElementById (String elementId)
  {
    return null;
  }
  /* #endif */

  public boolean hasAttributes ()
  {
    return false;
  }

  public String getInputEncoding ()
  {
    return null;
  }

  public String getXmlEncoding ()
  {
    return null;
  }

  public boolean getXmlStandalone ()
  {
    return false;
  }

  public void setXmlStandalone (boolean xmlStandalone)
  {
  }

  public String getXmlVersion ()
  {
    return "1.1";
  }

  public void setXmlVersion (String xmlVersion)
  {
  }

  public boolean getStrictErrorChecking ()
  {
    return false;
  }

  public void setStrictErrorChecking(boolean strictErrorChecking)
  {
  }

  public String getDocumentURI ()
  {
    return null;
  }

  public void setDocumentURI (String documentURI)
  {
  }

  /* #ifdef use:org.w3c.dom.Node */
  public Node renameNode (Node n, String namespaceURI, String qualifiedname)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR,
                           "renameNode not implemented");
  }

  public Node adoptNode (Node source)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR,
                           "adoptNode not implemented");
  }
  /* #endif */

  public void normalizeDocument ()
  {
  }

  /* #ifdef JAXP-1.3 */
  public DOMConfiguration getDomConfig ()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR,
                           "getDomConfig not implemented");
  }
  /* #endif JAXP-1.3 */
}

