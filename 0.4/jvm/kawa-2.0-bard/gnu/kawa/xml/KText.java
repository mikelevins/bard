// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
/* #ifdef use:org.w3c.dom.Node */
import org.w3c.dom.*;
/* #endif */

public class KText extends KCharacterData
  /* #ifdef use:org.w3c.dom.Node */
  implements org.w3c.dom.Text
  /* #endif */
{
  public KText (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public static KText make (String text)
  {
    NodeTree tree = new NodeTree();
    tree.append(text);
    return new KText(tree, 0);
  }

  /* #ifdef use:org.w3c.dom.Node */
  public short getNodeType () { return Node.TEXT_NODE; }
  /* #endif */

  public String getNodeName()
  {
    return "#text";
  }

  /* #ifdef use:org.w3c.dom.Node */
  public Text splitText(int offset)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "splitText not supported");
  }
  /* #endif */

  public String getWholeText ()
  {
    throw new UnsupportedOperationException("getWholeText not implemented yet");
  }

  /* #ifdef use:org.w3c.dom.Node */
  public Text replaceWholeText (String content)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "splitText not supported");
  }
  /* #endif */

  public boolean hasAttributes ()
  {
    return false;
  }

  public boolean isElementContentWhitespace ()
  {
    return false;
  }
}
