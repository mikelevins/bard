// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A SeqPosition for sequences that need more than a Pos int for a position.
 * For such sequences, a Pos int is an index into a PositionManager,
 * which manages a table of ExtPositions, which may contain more state
 * than a regular SeqPosition does.
 */

public class ExtPosition<E,ESEQ extends AbstractSequence<E>> extends SeqPosition<E,ESEQ>
{
  /** Index into PositionManager.positions, if >= 0.
   * This is used if we need a single Pos integer for this position. */
  int position = -1;

  public int getPos ()
  {
    if (position < 0)
      position = PositionManager.manager.register(this);
    return position;
  }

  public void setPos (AbstractSequence seq, int ipos)
  {
    throw seq.unsupported("setPos");
  }

  public final boolean isAfter()
  {
    return (ipos & 1) != 0;
  }

  public void release ()
  {
    if (position >= 0)
      PositionManager.manager.release(position);
    sequence = null;
  }
}
