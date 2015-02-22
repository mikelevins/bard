// Copyright (c) 2001, 2008  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/**
 * An array with a gap in the middle, allowing efficient insert and delete.
 * The actual storage is delegated to a SimpleVector, so the element
 * type depends on the sub-class of SimpleVector used.
 */

public class GapVector<E> extends AbstractSequence<E> implements Sequence<E>
{
  public SimpleVector<E> base;
  public int gapStart;
  public int gapEnd;

  public GapVector(SimpleVector<E> base)
  {
    this.base = base;
    this.gapStart = 0;
    this.gapEnd = base.size;
  }

  protected GapVector ()
  {
  }

  public int size()
  {
    return base.size - (gapEnd - gapStart);
  }

  public boolean hasNext(int ipos)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return index < base.size;
  }

  public int getNextKind(int ipos)
  {
    return hasNext(ipos) ? base.getElementKind() : EOF_VALUE;
  }

  @Override
  public E get(int index)
  {
    // If index is out of bounds, the base.get will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return base.get(index);
  }

  @Override
  public E set(int index, E value)
  {
    // If index is out of bounds, the base.set will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return base.set(index, value);
  }

  @Override
  public void fill(E value)
  {
    base.fill(gapEnd, base.size, value);
    base.fill(0, gapStart, value);
  }

  @Override
  public void fillPosRange(int fromPos, int toPos, E value)
  {
    int from = fromPos == -1 ? base.size : fromPos >>> 1;
    int to = toPos == -1 ? base.size : toPos >>> 1;
    int limit = gapStart < to ? gapStart : to;
    for (int i = from;  i < limit;  i++)
      base.setBuffer(i, value);
    for (int i = gapEnd;  i < to;  i++)
      base.setBuffer(i, value);
  }

  protected void shiftGap(int newGapStart)
  {
    int delta = newGapStart - gapStart;
    if (delta > 0)
      base.shift(gapEnd, gapStart, delta);
    else if (delta < 0)
      base.shift(newGapStart, gapEnd + delta, - delta);
    gapEnd += delta;
    gapStart = newGapStart;
  }

  /** Make sure gap is at least 'size' elements long. */
  protected final void gapReserve(int size)
  {
    gapReserve(gapStart, size);
  }

  /** Adjust gap to 'where', and make sure it is least `needed'
   * elements long. */
  protected void gapReserve(int where, int needed)
  {
    /* DEBUG
    int sz = size();
    Object[] values = new Object[sz];
    for (int i = 0;  i < sz;  i++)
      values[i] = get(i);
    */

    if (needed > gapEnd - gapStart)
      {
        // Need to resize.
	int oldLength = base.size;
        int newLength = oldLength < 16 ? 16 : 2 * oldLength;
        int size = oldLength - (gapEnd - gapStart);
	int minLength = size + needed;
	if (newLength < minLength)
	  newLength = minLength;
        int newGapEnd = newLength - size + where;
        base.resizeShift(gapStart, gapEnd, where, newGapEnd);
        gapStart = where;
        gapEnd = newGapEnd;
      }
    else if (where != gapStart)
      shiftGap(where);

    /* DEBUG
    sz = size();
    if (sz != values.length)
      throw new Error("gapReserve error");
    for (int i = 0;  i < sz;  i++) {
      Object val = get(i);
      if (values[i] == val)
        continue;
      if (val == null || values[i] == null
          || ! (val.equals(values[i])))
        throw new Error("gapReserve error");
    }
    */
  }

  /** If needed, move the gap so the given segment is contiguous.
   * @return the offset in the base array containing the segment,
   * or -1 if the parameters are bad.
   */
   
  public int getSegment (int where, int len)
  {
    int length = size();
    if (where < 0 || where > length)
      return -1;
    if (len < 0)
      len = 0;
    else if (where + len > length)
      len = length - where;
    // if (len < 0 || where + len > length)
    //   return -1;
    if (where + len <= gapStart)
      return where;
    if (where >= gapStart)
      return where + (gapEnd - gapStart);
    // Shift the gap depending in which direction needs least copying.
    if (gapStart - where > (len >> 1))
      {
	shiftGap(where + len);
	return where;
      }
    else
      {
	shiftGap(where);
	return where + (gapEnd - gapStart);
      }
  }

  @Override
  protected int addPos(int ipos, E value)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    add(index, value);
    return ((index + 1) << 1) | 1;
  }

  @Override
  public void add(int index, E o)
  {
    gapReserve(index, 1);
    base.setBuffer(index, o);
    gapStart++;
  }

  protected void removePosRange(int ipos0, int ipos1)
  {
    ipos0 >>>= 1;
    ipos1 >>>= 1;
    if (ipos0 > gapEnd)
      shiftGap(ipos0-gapEnd+gapStart);
    else if (ipos1 < gapStart)
      shiftGap(ipos1);
    if (ipos0 < gapStart)
      {
	base.clearBuffer(ipos0, gapStart - ipos0);
	gapStart = ipos0;
      }
    if (ipos1 > gapEnd)
      {
	base.clearBuffer(gapEnd, ipos1 - gapEnd);
	gapEnd = ipos1;
      }
  }

  /* FIXME - ths is quite wrong!
  public Object remove(int index)
  {
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return base.remove(index);
  }
  */

  public int createPos(int index, boolean isAfter)
  {
    if (index > gapStart)
      index += gapEnd - gapStart;
    // if (index == gapStart && isAfter) index = gapEnd; ??
    return (index << 1) | (isAfter ? 1 : 0);
  }

  protected boolean isAfterPos(int ipos)
  {
    return (ipos & 1) != 0;
  }

  protected int nextIndex(int ipos)
  {
    int index = ipos == -1 ? base.size : ipos >>> 1;
    if (index > gapStart)
      index -= gapEnd - gapStart;
    return index;
  }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (i < gapStart)
      {
	int lim = end < gapStart ? end : gapStart;
	base.consumePosRange(iposStart, lim << 1, out);
      }
    if (end > gapEnd)
      {
	i = i < gapEnd ? gapEnd : i;
	base.consumePosRange(i << 1, iposEnd, out);
      }
  }

}
