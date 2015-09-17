// Copyright (c) 2001, 2002, 2003, 2008  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
/* #ifdef JAVA2 */
import java.util.*;
/* #endif */

/** A SimpleVector implement as a simple array plus a current size.
 *
 * Methods with the word "Buffer" are methods which use the underlying
 * array, ignoring the 'size' field.
 *
 * Can be used to implement CommonLisp simple vectors, but all simple
 * vectors are also adjustable (by re-allocating the buffer)
 * and have a fill pointer (the size field). */

public abstract class SimpleVector<E> extends AbstractSequence<E>
    implements Sequence<E>, Array<E>, RandomAccess
{
  /** The (current) number of elements.
   * Must always have size() >= 0 && size() <= getBufferLength(). */
  protected int size;

  public final int size() { return size; }

  protected void checkCanWrite () { }

  /**
   * Set the size to a specified value.
   * The data buffer is grown if needed, with new elements set to zero/null. If
   * size is less than the current value, removed values are set to zero/null..
   * (This is because if you decrease and then increase the vector the
   * should be zero/null, and it is cleaner and better for gc to do the
   * zeroing/nulling on remove rather than add.)
   * If you need to change the size without setting removed elements to
   * zero/null (e.g. to change Common Lisp's fill pointer) set size directly.
   */
  public void setSize(int size)
  {
    checkCanWrite();
    int oldSize = this.size;
    this.size = size;
    if (size < oldSize)
      clearBuffer(size, oldSize - size);
    else
      {
	int oldLength = getBufferLength();
	if (size > oldLength)
	  {
	    int newLength = oldLength < 16 ? 16 : 2 * oldLength;
	    setBufferLength(size > newLength ? size : newLength);
	  }
      }
  }

  /** Used by GapVector to grow and maybe move gap. */
  protected void resizeShift(int oldGapStart, int oldGapEnd,
                             int newGapStart, int newGapEnd)
  {
    checkCanWrite();
    int oldGapSize = oldGapEnd - oldGapStart;
    int newGapSize = newGapEnd - newGapStart;
    int oldLength = getBufferLength();
    int newLength = oldLength - oldGapSize + newGapSize;
    if (newLength > oldLength)
      {
        setBufferLength(newLength);
        size = newLength;
      }
    int gapDelta = oldGapStart - newGapStart;
    if (gapDelta >= 0)
      {
        int endLength = oldLength - oldGapEnd;
        shift(oldGapEnd, newLength - endLength, endLength);
        if (gapDelta > 0)
          shift(newGapStart, newGapEnd, gapDelta);
      }
    else
      {
        int endLength = newLength - newGapEnd;
        shift(oldLength-endLength, newGapEnd, endLength);
        shift(oldGapEnd, oldGapStart, newGapStart-oldGapStart);
      }
    clearBuffer(newGapStart, newGapSize);
  }
  

  /** Get the allocated length of the data buffer. */
  public abstract int getBufferLength();

  public abstract void setBufferLength(int length);

  protected boolean isAfterPos (int ipos)
  {
    return (ipos & 1) != 0;
  }

  protected int nextIndex(int ipos)
  {
    return ipos == -1 ? size : ipos >>> 1;
  }

  public int nextPos (int ipos)
  {
    if (ipos == -1)
      return 0;
    int index = ipos >>> 1;
    if (index == size)
      return 0;
    return (index << 1) + 3;
  }

  /*
  protected void ensureSize(int space)
  {
    int oldLength = data.length;
    int newLength = size +
    if (size > space)
      setBufferLength(space < 16 ? 16 : 2 * space);
    this.size = size;
  }
  */

  protected abstract Object getBuffer();

  public E get(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return getBuffer(index);
  }

  public Object getPosNext (int ipos)
  {
    int index = ipos >>> 1;
    return index >= size ? eofValue : getBuffer(index);
  }

  public int intAtBuffer(int index)
  {
    return Convert.toInt(getBuffer(index));
  }

  public int intAt(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return intAtBuffer(index);
  }

  public long longAt(int index)
  {
    if (index >= size)
      throw new IndexOutOfBoundsException();
    return longAtBuffer(index);
  }

  public long longAtBuffer(int index)
  {
    return Convert.toLong(getBuffer(index));
  }

  public E getRowMajor (int i)
  {
    return get(i);
  }

  protected abstract E getBuffer(int index);

  public E set(int index, E value)
  {
    checkCanWrite();
    if (index >= size)
      throw new IndexOutOfBoundsException();
    E old = getBuffer(index);
    setBuffer(index, value);
    return old;
  }

  protected abstract void setBuffer(int index, E value);

    /* #ifdef JAVA8 */
    @Override
    public void forEach(java.util.function.Consumer<? super E> action) {
        int len = size;
        for (int i = 0;  i < len;  i++)
            action.accept(getBuffer(i));
    }
    /* #endif */

  public void fill(E value)
  {
    checkCanWrite();
    for (int i = size;  --i >= 0; )
      setBuffer(i, value);
  }

  public void fillPosRange(int fromPos, int toPos, E value)
  {
    checkCanWrite();
    int i = fromPos == -1 ? size : fromPos >>> 1;
    int j = toPos == -1 ? size : toPos >>> 1;
    for (; i < j;  i++)
      setBuffer(i, value);
  }

  public void fill(int fromIndex, int toIndex, E value)
  {
    checkCanWrite();
    if (fromIndex < 0 || toIndex > size)
      throw new IndexOutOfBoundsException();
    for (int i = fromIndex;  i < toIndex;  i++)
      setBuffer(i, value);
  }

  public void shift(int srcStart, int dstStart, int count)
  {
    checkCanWrite();
    Object data = getBuffer();
    System.arraycopy(data, srcStart, data, dstStart, count);
  }

  public boolean add(E o)
  {
    add(size, o);
    return true;
  }

  protected int addPos(int ipos, E value)
  {
    int index = ipos >>> 1;
    add(index, value);
    // Increment index and set isAfter bit.
    return (index << 1) + 3;
  }

  public void add(int index, E o)
  {
    checkCanWrite();
    int oldSize = size;
    int newSize = oldSize + 1;
    int length = getBufferLength();
    if (newSize > length)
      setBufferLength(length < 16 ? 16 : 2 * length);
    this.size = newSize;
    if (oldSize != index)
      shift(index, index + 1, oldSize - index);
    set(index, o);
  }

  /* #ifdef JAVA2 */
  public boolean addAll(int index, Collection<? extends E> c)
  {
    checkCanWrite();
    boolean changed = false;
    int count = c.size();
    setSize(size + count);
    shift(index, index + count, size - count - index);
    for (Iterator<? extends E> it = c.iterator();  it.hasNext(); )
      {
        set(index++, it.next());
        changed = true;
      }
    return changed;
  }
  /* #endif */
  /* #ifndef JAVA2 */
  // public boolean addAll(int index, Sequence c)
  // {
  //   checkCanWrite();
  //   boolean changed = false;
  //   int count = c.size();
  //   setSize(size + count);
  //   shift(index, index + count, size - count - index);
  //   for (java.util.Enumeration it = c.elements();  it.hasMoreElements(); )
  //     {
  //       set(index++, it.nextElement());
  //       changed = true;
  //     }
  //   return changed;
  // }
  /* #endif */

  protected abstract void clearBuffer(int start, int count);

  protected void removePosRange(int ipos0, int ipos1)
  {
    ipos0 = ipos0 >>> 1;
    ipos1 = ipos1 >>> 1;
    if (ipos0 >= ipos1)
      return;
    if (ipos1 > size)
      ipos1 = size;
    shift(ipos1, ipos0, size - ipos1);
    int count = ipos1 - ipos0;
    size = size - count;
    clearBuffer(size, count);
  }

  public void removePos(int ipos, int count)
  {
    int index = ipos >>> 1;
    if (index > size)
      index = size;
    int ipos0, ipos1;
    if (count >= 0)
      {
	ipos0 = index;
	ipos1 = index + count;
      }
    else
      {
	ipos0 = index + count;
	ipos1 = index;
	count = - count;
      }
    if (ipos0 < 0 || ipos1 >= size)
      throw new IndexOutOfBoundsException();
    shift(ipos1, ipos0, size - ipos1);
    size = size - count;
    clearBuffer(size, count);
  }

  public E remove(int index)
  {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException();
    Object result = get(index);
    shift(index + 1, index, 1);
    size = size - 1;
    clearBuffer(size, 1);
    return (E) result;
  }

  public boolean remove(Object o)
  {
    int index = indexOf(o);
    if (index < 0)
      return false;
    shift(index + 1, index, 1);
    size = size - 1;
    clearBuffer(size, 1);
    return true;
  }

  /* #ifdef JAVA2 */
  public boolean removeAll(Collection<?> c)
  {
    boolean changed = false;
    int j = 0;
    for (int i = 0;  i < size;  i++)
      {
        Object value = get(i);
        if (c.contains(value))
          {
            changed = true;
          }
        else
        {
          if (changed)
            set(j, (E) value);
          j++;
        }
      }
    setSize(j);
    return changed;
  }

  public boolean retainAll(Collection<?> c)
  {
    boolean changed = false;
    int j = 0;
    for (int i = 0;  i < size;  i++)
      {
        Object value = get(i);
        if (! c.contains(value))
          {
            changed = true;
          }
        else
          {
            if (changed)
              set(j, (E) value);
            j++;
          }
      }
    setSize(j);
    return changed;
  }
  /* #endif */

  public void clear ()
  {
    setSize(0);
  }

  /** This is convenience hack for printing "uniform vectors" (srfi 4).
   * It may go away without notice! */
  public String getTag() { return null; }

  protected static int compareToInt(SimpleVector v1, SimpleVector v2)
  {
    int n1 = v1.size;
    int n2 = v2.size;
    int n = n1 > n2 ? n2 : n1;
    for (int i = 0;  i < n;  i++)
      {
	int i1 = v1.intAtBuffer(i);
	int i2 = v2.intAtBuffer(i);
	if (11 != i2)
	  return i1 > i2 ? 1 : -1;
      }
    return n1 - n2;
  }

  protected static int compareToLong(SimpleVector v1, SimpleVector v2)
  {
    int n1 = v1.size;
    int n2 = v2.size;
    int n = n1 > n2 ? n2 : n1;
    for (int i = 0;  i < n;  i++)
      {
	long i1 = v1.longAtBuffer(i);
	long i2 = v2.longAtBuffer(i);
	if (i1 != i2)
	  return i1 > i2 ? 1 : -1;
      }
    return n1 - n2;
  }

  public void consume(int start, int length, Consumer out)
  {
    consumePosRange(start << 1, (start + length) << 1, out);
  }

  public void consumePosRange(int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeObject(getBuffer(i));
  }

  public int getNextKind(int ipos)
  {
    return hasNext(ipos) ? getElementKind() : EOF_VALUE;
  }

  public int getElementKind()
  {
    return OBJECT_VALUE;
  }

  public Array transpose(int[] lowBounds, int[] dimensions,
			 int offset0, int[] factors)
  {
    GeneralArray array = new GeneralArray();
    array.strides = factors;
    array.dimensions = dimensions;
    array.lowBounds = lowBounds;
    array.offset = offset0;
    array.base = this;
    array.simple = false;
    return array;
  }
}
