// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.kawa.xml.*;

/** Used to filter the output of RelativeStep.
 * Atomic values are passed though as-is, while node values are sorted
 * by document order and duplicates removed.  An exception is thrown
 * if there is a mix of atoms and nodes.
 * Informally: {@code E1/E2} is implemented as:
 * {@code RelativeStepFilter(for $dot in E1 return E2)}.
 */

public class RelativeStepFilter extends FilterConsumer
  implements PositionConsumer
{
  // 'A' for atomic, 'N' for nodes, '\0' for neither.
  char seen;

  SortedNodes snodes;

  public RelativeStepFilter (Consumer base)
  {
    super(base);
  }

  // Not sure if this is ever called ...
  public void writePosition(SeqPosition position)
  {
    writePosition(position.sequence, position.ipos);
  }

  public void writeObject(Object v)
  {
    if (v instanceof SeqPosition)
      {
        SeqPosition n = (SeqPosition) v;
        writePosition(n.sequence, n.ipos);
      }
    else
      super.writeObject(v);
  }

  protected void beforeContent ()
  {
    if (seen == 'N')
      throw new Error("path returns mix of atoms and nodes");
    seen = 'A';
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    if (seen == 'A')
      throw new Error("path returns mix of atoms and nodes");
    seen = 'N';
    if (snodes == null)
      snodes = new SortedNodes();
    snodes.writePosition(seq, ipos);
  }

  public void finish ()
  {
    if (snodes != null)
      snodes.consume(base);
    snodes = null;
  }
}
