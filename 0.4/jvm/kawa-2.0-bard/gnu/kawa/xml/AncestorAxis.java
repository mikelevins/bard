// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a ancestor:: step in a path expression. */

public class AncestorAxis extends TreeScanner
{
  public static AncestorAxis make (NodePredicate type)
  {
    AncestorAxis axis = new AncestorAxis();
    axis.type = type;
    return axis;
  }

  private static void scan (AbstractSequence seq, int ipos, int end,
			    NodePredicate type, PositionConsumer out)
  {
    ipos = seq.parentPos(ipos);
    if (ipos != end)
      {
	scan(seq, ipos, end, type, out);
        if (type.isInstancePos(seq, ipos))
          out.writePosition(seq, ipos);
      }
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  { 
    scan(seq, ipos, seq.endPos(), type, out);
  }
}
