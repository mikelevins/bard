// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a following-sibling:: step in a path expression. */

public class PrecedingSiblingAxis extends TreeScanner
{
  public static PrecedingSiblingAxis make (NodePredicate type)
  {
    PrecedingSiblingAxis axis = new PrecedingSiblingAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    int end = seq.endPos();
    int parent = seq.parentPos(ipos);
    if (parent == end)
      return;
    int child = seq.firstChildPos(parent);
    if (child == 0)
      return;
    if (type.isInstancePos(seq, child))
      out.writePosition(seq, child);
    for (;;)
      {
	child = seq.nextMatching(child, type, ipos, false);
	if (child == 0)
	  break;
	out.writePosition(seq, child);
      }
  }
}
