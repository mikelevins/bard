// Copyright (c) 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** Abstract helper class for Sequences that use an ExtPosition.
 * That is sequences where it is inefficient to represent a position
 * just using a Pos int.
 */

public abstract class ExtSequence<E> extends AbstractSequence<E>
{
  public int copyPos (int ipos)
  { 
    if (ipos <= 0)
      return ipos;
    return PositionManager.manager.register(PositionManager.getPositionObject(ipos).copy());
  }

  protected void releasePos(int ipos)
  {
    if (ipos > 0)
      PositionManager.manager.release(ipos);
  }

  protected boolean isAfterPos (int ipos)
  {
    if (ipos <= 0)
      return ipos < 0;
    return (PositionManager.getPositionObject(ipos).ipos & 1) != 0;
  }

  protected int nextIndex(int ipos)
  {
    return ipos == -1 ? size() : ipos == 0 ? 0
      : PositionManager.getPositionObject(ipos).nextIndex();
  }
}
