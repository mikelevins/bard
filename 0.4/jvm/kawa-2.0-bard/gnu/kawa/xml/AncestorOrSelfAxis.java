// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a ancestor-or-self:: step in a path expression. */

public class AncestorOrSelfAxis extends TreeScanner
{
  public static AncestorOrSelfAxis make (NodePredicate type)
  {
    AncestorOrSelfAxis axis = new AncestorOrSelfAxis();
    axis.type = type;
    return axis;
  }

  private static void scan (AbstractSequence seq, int ipos, int end,
			    NodePredicate type, PositionConsumer out)
  {
    if (ipos != end)
      {
	scan(seq, seq.parentPos(ipos), end, type, out);
        if (type.isInstancePos(seq, ipos))
          out.writePosition(seq, ipos);
      }
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  { 
    int end = seq.endPos();
    scan(seq, ipos, end, type, out);
  }
}
