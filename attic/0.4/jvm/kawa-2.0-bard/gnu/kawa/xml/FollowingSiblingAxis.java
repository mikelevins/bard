// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a following-sibling:: step in a path expression. */

public class FollowingSiblingAxis extends TreeScanner
{
  public static FollowingSiblingAxis make (NodePredicate type)
  {
    FollowingSiblingAxis axis = new FollowingSiblingAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    int limit = seq.endPos();
    for (;;)
      {
	ipos = seq.nextMatching(ipos, type, limit, false);
	if (ipos == 0)
	  break;
	out.writePosition(seq, ipos);
      }
  }
}
