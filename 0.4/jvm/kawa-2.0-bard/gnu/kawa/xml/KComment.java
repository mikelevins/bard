// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
/* #ifdef use:org.w3c.dom.Node */
import org.w3c.dom.*;
/* #endif */

public class KComment extends KCharacterData
  /* #ifdef use:org.w3c.dom.Node */
  implements org.w3c.dom.Comment
  /* #endif */
{
  public KComment (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  /* #ifdef use:org.w3c.dom.Node */
  public short getNodeType () { return Node.COMMENT_NODE; }
  /* #endif */

  public String getNodeName()
  {
    return "#comment";
  }

  public static KComment valueOf (String text)
  {
    NodeTree tree = new NodeTree();
    tree.writeComment(text, 0, text.length());
    return new KComment(tree, 0);
  }
}
