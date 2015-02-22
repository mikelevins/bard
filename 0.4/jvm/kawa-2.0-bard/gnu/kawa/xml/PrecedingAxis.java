// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a following:: step in a path expression. */

public class PrecedingAxis extends TreeScanner
{
  public static PrecedingAxis make (NodePredicate type)
  {
    PrecedingAxis axis = new PrecedingAxis();
    axis.type = type;
    return axis;
  }

  private static void scan (AbstractSequence seq, int ipos, int end,
			    NodePredicate type, PositionConsumer out)
  {
    int parent = seq.parentPos(ipos);
    if (parent == end)
      return;
    scan (seq, parent, ipos, type, out);
    int child = seq.firstChildPos(parent);
    if (child == 0 || child == ipos)
      return;
    if (type.isInstancePos(seq, child))
      out.writePosition(seq, child);
    for (;;)
      {
	child = seq.nextMatching(child, type, ipos, true);
	if (child == 0)
	  break;
	out.writePosition(seq, child);
      }
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    scan(seq, ipos, seq.endPos(), type, out);
  }
}

