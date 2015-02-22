// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a self:: step in a path expression. */

public class SelfAxis extends TreeScanner
{
  public static SelfAxis make (NodePredicate type)
  {
    SelfAxis axis = new SelfAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    if (type.isInstancePos(seq, ipos))
      out.writePosition(seq, ipos);
  }
}
