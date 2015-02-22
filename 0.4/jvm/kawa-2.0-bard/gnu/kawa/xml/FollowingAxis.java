// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a following:: step in a path expression. */

public class FollowingAxis extends TreeScanner
{
  public static FollowingAxis make (NodePredicate type)
  {
    FollowingAxis axis = new FollowingAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    int limit = seq.endPos();
    ipos = seq.nextPos(ipos);
    if (ipos != 0 && type.isInstancePos(seq, ipos))
      out.writePosition(seq, ipos);
    for (;;)
      {
	ipos = seq.nextMatching(ipos, type, limit, true);
	if (ipos == 0)
	  break;
	out.writePosition(seq, ipos);
      }
  }
}
