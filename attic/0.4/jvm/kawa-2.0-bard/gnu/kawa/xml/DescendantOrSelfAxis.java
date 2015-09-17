// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a descendant-or-self:: step in a path expression. */

public class DescendantOrSelfAxis extends TreeScanner
{
  public static final DescendantOrSelfAxis anyNode =
    new DescendantOrSelfAxis(NodeType.anyNodeTest);

  private DescendantOrSelfAxis (NodePredicate type)
  {
    this.type = type;
  }

  public static DescendantOrSelfAxis make (NodePredicate type)
  {
    if (type == NodeType.anyNodeTest)
      return anyNode;
    return new DescendantOrSelfAxis(type);
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    if (type.isInstancePos(seq, ipos))
      out.writePosition(seq, ipos);
    if (! (seq instanceof TreeList))
      { // AbstractSequence's nextMatching does not support descend.  FIXME.
	ipos = seq.firstChildPos(ipos);
	while (ipos != 0)
	  {
	    scan(seq, ipos, out);
	    ipos = seq.nextPos(ipos);
	  }
	return;
      }
    int limit = seq.nextPos(ipos);
    int child = ipos;
    for (;;)
      {
	child = seq.nextMatching(child, type, limit, true);
	if (child == 0)
	  break;
	out.writePosition(seq, child);
      }
  }
}
