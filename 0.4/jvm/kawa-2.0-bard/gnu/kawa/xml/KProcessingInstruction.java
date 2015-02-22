// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
/* #ifdef use:org.w3c.dom.Node */
import org.w3c.dom.*;
/* #endif */

public class KProcessingInstruction extends KNode
  /* #ifdef use:org.w3c.dom.Node */
  implements org.w3c.dom.ProcessingInstruction
  /* #endif */
{
  public KProcessingInstruction (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  /* #ifdef use:org.w3c.dom.Node */
  public short getNodeType () { return Node.PROCESSING_INSTRUCTION_NODE; }
  /* #endif */

  public String getNodeName()
  {
    return getTarget();
  }

  public String getData ()
  {
    return getNodeValue();
  }

  /* #ifdef use:org.w3c.dom.Node */
  public void setData(String data)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
                           "setData not supported");
  }
  /* #endif */

  public String getTarget ()
  {
    return ((NodeTree) sequence).posTarget(ipos);
  }

  public static KProcessingInstruction valueOf (String target, String content)
  {
    NodeTree tree = new NodeTree();
    tree.writeProcessingInstruction(target, content, 0, content.length());
    return new KProcessingInstruction(tree, 0);
  }
}
