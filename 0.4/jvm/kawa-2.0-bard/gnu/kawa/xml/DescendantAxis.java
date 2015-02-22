// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a descendant:: step in a path expression. */

public class DescendantAxis extends TreeScanner
{
  public static DescendantAxis make (NodePredicate type)
  {
    DescendantAxis axis = new DescendantAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    if (! (seq instanceof TreeList))
      { // AbstractSequence's nextMatching does not support descend.  FIXME.
	ipos = seq.firstChildPos(ipos);
	while (ipos != 0)
	  {
	    if (type.isInstancePos(seq, ipos))
	      out.writePosition(seq, ipos);
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
