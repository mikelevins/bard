// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a parent:: step in a path expression. */

public class ParentAxis extends TreeScanner
{
  public static ParentAxis make (NodePredicate type)
  {
    ParentAxis axis = new ParentAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    ipos = seq.parentPos(ipos);
    int end = seq.endPos();
    if (ipos != end && type.isInstancePos(seq, ipos))
      out.writePosition(seq, ipos);
  }
}
