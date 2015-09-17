// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement an attribute:: step in a path expression. */

public class AttributeAxis extends TreeScanner
{
  public static AttributeAxis make (NodePredicate type)
  {
    AttributeAxis axis = new AttributeAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    ipos = seq.firstAttributePos(ipos);
    while (ipos != 0 && seq.getNextKind(ipos) == Sequence.ATTRIBUTE_VALUE)
      {
	if (type.isInstancePos(seq, ipos))
	  out.writePosition(seq, ipos);
        else if (seq.getNextKind(ipos) != Sequence.ATTRIBUTE_VALUE)
          break;
	ipos = seq.nextPos(ipos);
      }
  }
}
