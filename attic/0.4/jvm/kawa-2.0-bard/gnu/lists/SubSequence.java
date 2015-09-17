// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A sequence consisting of a sub-range of the elements of a base sequence.
 * The start and end positions are position triples (on the same sequence).
 */

public class SubSequence<E>
extends AbstractSequence<E> implements Sequence<E>
{
  /** Normally the Sequence this a sub-sequence of.
   * Actually the sequence that provides context for the
   * start and end position pairs. */
  AbstractSequence<E> base;

  /** Integer part of start position. */
  int ipos0;

  /** Integer part of end position. */
  int ipos1;

  public SubSequence()
  {
  }

  public SubSequence(AbstractSequence<E> base, int startPos, int endPos)
  {
    this.base = base;
    this.ipos0 = startPos;
    this.ipos1 = endPos;
  }

  public SubSequence(AbstractSequence<E> base)
  {
    this.base = base;
  }

  @Override
  public E get(int index)
  {
    if (index < 0 || index >=size())
      throw new IndexOutOfBoundsException();
    int start = base.nextIndex(ipos0);
    return base.get(start + index);
  }

  public int size()
  {
    return base.getIndexDifference(ipos1, ipos0);
  }

  public void removePosRange(int istart, int iend)
  {
    base.removePosRange(istart == 0 ? ipos0 : istart == -1 ? ipos1 : istart,
			iend == -1 ? ipos1 : iend == 0 ? ipos0 : iend);
  }

  protected boolean isAfterPos(int ipos)
  { return base.isAfterPos(ipos); }

  public int createPos(int offset, boolean isAfter)
  {
    return base.createRelativePos(ipos0, offset, isAfter);
  }

  public int createRelativePos(int pos, int offset, boolean isAfter)
  {
    return base.createRelativePos(pos, offset, isAfter);
  }

  protected int getIndexDifference(int ipos1, int ipos0)
  {
    return base.getIndexDifference(ipos1, ipos0);
  }

  public void releasePos(int ipos)
  {
    base.releasePos(ipos);
  }

  protected int nextIndex (int ipos)
  {
    return getIndexDifference(ipos, ipos0);
  }

  public int compare (int ipos1, int ipos2)
  {
    return base.compare(ipos1, ipos2);
  }

  public Object getPosNext(int ipos)
  {
    if (base.compare(ipos, ipos1) >= 0)
      return eofValue;
    return base.getPosNext(ipos);
  }

  public int getNextKind(int ipos)
  {
    if (base.compare(ipos, ipos1) >= 0)
      return EOF_VALUE;
    return base.getNextKind(ipos);
  }

  public int startPos () { return ipos0; }
  public int endPos () { return ipos1; }

  public Object getPosPrevious(int ipos)
  {
    if (base.compare(ipos, ipos0) <= 0)
      return eofValue;
    return base.getPosPrevious(ipos);
  }

  @Override
  protected Sequence<E> subSequencePos(int ipos0, int ipos1)
  {
    return new SubSequence<E>(base, ipos0, ipos1);
  }

  public void clear()
  {
    removePosRange(ipos0, ipos1);
  }

  public void finalize()
  {
    base.releasePos(ipos0);
    base.releasePos(ipos1);
  }
}
