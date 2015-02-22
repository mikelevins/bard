// Copyright (c) 2001, 2002, 2003, 2005  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.util.*;

/**
 * An AbstractSequence is used to implement Sequences, and almost all
 * classes that extend AbstractSequence will implement Sequence.
 * However, AbstractSequence itself does not implement Sequence.
 * This is so we can use AbstractSequence to implement classes that are
 * "sequence-like" (such as multi-dimensional arrays) but are not Sequences.
 *
 * Additionally, a sequence may have zero or more attributes, which are
 * name-value pairs.  A sequence may also have a named "type".  These
 * extensions are to support XML functionality - it might be cleaner to
 * moe them to a sub-class of Sequence or some interface.
 *
 * Many of the protected methods in Sequence (such as nextIndex) are
 * only intended to be called from SeqPosition or TreePosition, see those.
 *
 * @author Per Bothner
 */

public abstract class AbstractSequence<E>
{
  /** See java.util.List. */
  public abstract int size();

  public boolean isEmpty()
  {
    return size() == 0;
  }

  public int rank()
  {
    return 1;
  }

  /** See java.util.List. */
  public abstract E get (int index);

  public int getEffectiveIndex(int[] indexes)
  {
    return indexes[0];
  }

  public E get(int[] indexes)
  {
    return get(indexes[0]);
  }

  public E set(int[] indexes, E value)
  {
    return set(indexes[0], value);
  }

  public int getLowBound(int dim)
  {
    return 0;
  }

  public int getSize (int dim)
  {
    return dim==0 ? size() : 1;
  }

  protected RuntimeException unsupported (String text)
  {
    return unsupportedException(getClass().getName()
                                + " does not implement " + text);
  }

  public static RuntimeException unsupportedException (String text)
  {
    /* #ifdef JAVA2 */
    return new UnsupportedOperationException(text);
    /* #endif */
    /* #ifndef JAVA2 */
    // return new RuntimeException(text);
    /* #endif */
  }

  public E set(int index, E element)
  {
    throw unsupported("set");
  }

  public void fill(E value)
  {
    for (int i = startPos(); (i = nextPos(i)) != 0; )
      setPosPrevious(i, value);
  }

  public void fillPosRange(int fromPos, int toPos, E value)
  {
    int i = copyPos(fromPos);
    for (;  compare(i, toPos) < 0;  i = nextPos(i))
      setPosNext(i, value);
    releasePos(i);
  }

  public void fill(int fromIndex, int toIndex, E value)
  {
    int i = createPos(fromIndex, false);
    int limit = createPos(toIndex, true);
    for (;  compare(i, limit) < 0;  i = nextPos(i))
      setPosNext(i, value);
    releasePos(i);
    releasePos(limit);
  }

  // FIXME?
  //public final Object elementAt (int index) { return get(index); }

  /** See java.util.List. */
  public int indexOf(Object o)
  {
    int i = 0;
    for (int iter = startPos();  (iter = nextPos(iter)) != 0;  i++)
      {
        Object v = getPosPrevious(iter);
        if (o==null ? v==null : o.equals(v))
	  {
	    releasePos(iter);
	    return i;
	  }
      }
    return -1;
  }

  /** See java.util.List. */
  public int lastIndexOf(Object o)
  {
    // FIXME use iterator?
    for (int n = size();  --n >= 0; )
      {
        Object e = get(n);
        if (o==null ? e == null : o.equals(e))
          return n;
      }
    return -1;
  }

  /** Get next matching child or descendent (ignoring attributes).
   * @param startPos starting position
   * @param type a test (predicate) to apply to selected elements
   * @param endPos stop before endPos
   * @param descend if true do depth-first traversal.
   * @return poistion of next match or 0 if none found
   */
  public int nextMatching(int startPos, ItemPredicate type,
			  int endPos, boolean descend)
  {
    if (descend)
      throw unsupported("nextMatching with descend");
    int ipos = startPos;
    for (;;)
      {
	if (compare(ipos, endPos) >= 0)
	  return 0;
	ipos = nextPos(ipos);
	if (type.isInstancePos(this, ipos))
	  return ipos;
      }
  }

  /** See java.util.List. */
  public boolean contains(Object o)
  {
    return indexOf(o) >= 0;
  }

  /* #ifdef JAVA2 */
  /** See java.util.List. */
  public boolean containsAll(Collection<?> c)
  {
    Iterator<?> i = c.iterator();
    while (i.hasNext())
      {
        Object e = i.next();
        if (! contains(e))
          return false;
      }
    return true;
  }
  /* #endif */

  public final Enumeration<E> elements()
  {
    return getIterator();
  }

  public final SeqPosition<E, AbstractSequence<E>> getIterator()
  {
    return getIterator(0);
  }

  public SeqPosition<E, AbstractSequence<E>> getIterator(int index)
  {
      return new SeqPosition<E,AbstractSequence<E>>(this, index, false);
  }

  public SeqPosition<E, AbstractSequence<E>> getIteratorAtPos(int ipos)
  {
    return new SeqPosition<E,AbstractSequence<E>>(this, copyPos(ipos));
  }

  /* #ifdef JAVA2 */
  public final Iterator<E> iterator()
  {
    return getIterator();
  }

  public final ListIterator<E> listIterator()
  {
    return getIterator(0);
  }

  public final ListIterator<E> listIterator(int index)
  {
    return getIterator(index);
  }
  /* #endif */

  /** Add a value at a specified Pos.
   * @return the updated Pos, which is after the inserted value..
   */
  protected int addPos (int ipos, E value)
  {
    throw unsupported("addPos");
  }

  /** See java.util.Collection. */
  public boolean add(E o)
  {
    addPos(endPos(), o);
    return true;
  }

  /** See java.util.List. */
  public void add(int index, E o)
  {
    int pos = createPos(index, false);
    addPos(pos, o);
    releasePos(pos);
  }

  /* #ifdef JAVA2 */
  /** See java.util.Collection. */
  public boolean addAll(Collection<? extends E> c)
  {
    return addAll(size(), c);
  }

  /** See java.util.Collection. */
  public boolean addAll(int index, Collection<? extends E> c)
  {
    boolean changed = false;
    int pos = createPos(index, false);
    for (Iterator<? extends E> it = c.iterator();  it.hasNext(); )
      {
        pos = addPos(pos, it.next());
        changed = true;
      }
    releasePos(pos);
    return changed;
  }
  /* #endif */
  /* #ifndef JAVA2 */
  // public boolean addAll(Sequence c)
  // {
  //   return addAll(size(), c);
  // }

  // public boolean addAll(int index, Sequence c)
  // {
  //   boolean changed = false;
  //   int pos = createPos(index, false);
  //   for (int iter = startPos();  (iter = nextPos(iter)) != 0; )
  //     {
  //       pos = addPos(pos, getPosPrevious(iter));
  //       changed = true;
  //     }
  //   releasePos(pos);
  //   return changed;
  // }
  /* #endif */

  /**
   * Remove one or more elements.
   * @param ipos position where elements should be removed
   * @param count if non-negative, remove that number of elements
   * following (poses, posNumber); if negative the negative of the number
   * of elements to remove before (poses, posNumber).
   * @exception java.lang.IndexOutOfBoundsException
   *   if (count >= 0 ? (index < 0 || index + count > size())
   *       : (index + count < 0 || index > size())),
   *   where index == nextIndex(ipos, xpos).
   */
  public void removePos(int ipos, int count)
  {
    int rpos = createRelativePos(ipos, count, false);
    if (count >= 0)
      removePosRange(ipos, rpos);
    else
      removePosRange(rpos, ipos);
    releasePos(rpos);
  }

  /** Remove a range where each end-point is a position in a container.
   * @param ipos0 start of range, as a poistion
   * @param ipos1 end of range
   * @exception java.lang.IndexOutOfBoundsException
   *   if nextIndex(ipos0) > nextIndex(ipos1)
   *   || nextIndex(ipos0) < 0 || nextIndex(ipos1) > size()
   */
  protected void removePosRange(int ipos0, int ipos1)
  {
    throw unsupported("removePosRange");
  }

  public E remove(int index)
  {
    if (index < 0 || index >= size())
      throw new IndexOutOfBoundsException();
    int ipos = createPos(index, false);
    E result = (E) getPosNext(ipos);
    removePos(ipos, 1);
    releasePos(ipos);
    return result;
  }

  public boolean remove(Object o)
  {
    int index = indexOf(o);
    if (index < 0)
      return false;
    int ipos = createPos(index, false);
    removePos(ipos, 1);
    releasePos(ipos);
    return true;
  }

  /* #ifdef JAVA2 */
  public boolean removeAll(Collection<?> c)
  {
    boolean changed = false;
    for (int iter = startPos();  (iter = nextPos(iter)) != 0; )
      {
        Object value = getPosPrevious(iter);
        if (c.contains(value))
          {
            removePos(iter, -1);
            changed = true;
          }
      }
    return changed;
  }

  public boolean retainAll(Collection<?> c)
  {
    boolean changed = false;
    for (int iter = startPos();  (iter = nextPos(iter)) != 0; )
      {
        Object value = getPosPrevious(iter);
        if (! c.contains(value))
          {
            removePos(iter, -1);
            changed = true;
          }
      }
    return changed;
  }
  /* #endif */

  public void clear()
  {
    removePos(startPos(), endPos());
  }

  /** Tests whether the position has the "isAfter" property.
   * I.e. if something is inserted at the position, will
   * the iterator end up being after the new data? */
  protected boolean isAfterPos (int ipos)
  {
    return (ipos & 1) != 0;
  }

  /** Generate a position at a given index.
   * The result is a position cookie that must be free'd with releasePos.
   * @param index offset from beginning of desired position
   * @param isAfter should the position have the isAfter property
   * @exception IndexOutOfBoundsException if index is out of bounds
   */
  public int createPos (int index, boolean isAfter) {
      return (index << 1) | (isAfter ? 1 : 0);
  }

  public int createRelativePos(int pos, int delta, boolean isAfter)
  {
    return createPos(nextIndex(pos) + delta, isAfter);
  }

  public int startPos () { return 0; }
  public int endPos () { return -1; }

  /**
   * Reclaim any resources used by the given position int.
   * @param ipos the Pos being free'd.
   */
  protected void releasePos(int ipos)
  {
  }

  /** Make a copy of a position int.
   * For simple positions returns the argument.
   * However, if the positions are magic cookies that are actively managed
   * by the sequence (as opposed to for example a simple index), then making
   * a copy may need to increment a reference count, or maybe allocate a
   * new position cookie.  In any case, the new position is initialized to
   * the same offset (and isAfter property) as the original.
   * @param ipos the position being copied.
   * @return the new position
   */
  public int copyPos (int ipos)
  {
    return ipos;
  }

  /** Get offset of (ipos1) relative to (ipos0). */
  protected int getIndexDifference(int ipos1, int ipos0)
  {
    return nextIndex(ipos1) - nextIndex(ipos0);
  }

  /**
   * Get the offset from the beginning corresponding to a position cookie.
   */
  protected abstract int nextIndex(int ipos);

  protected int fromEndIndex(int ipos)
  {
    return size() - nextIndex(ipos);
  }

  /**
   * Get the size of the (sub-) sequence containing a given position.
   * Normally the same as size(), but may be different if this Sequence
   * is a tree and the position points at an interior node.
   */
  protected int getContainingSequenceSize(int ipos)
  {
    return size();
  }

  public boolean hasNext (int ipos)
  {
    return nextIndex(ipos) != size();
  }

  public int getNextKind(int ipos)
  {
    return hasNext(ipos) ? Sequence.OBJECT_VALUE : Sequence.EOF_VALUE;
  }

  public String getNextTypeName(int ipos)
  {
    Object type = getNextTypeObject(ipos);
    return type == null ? null : type.toString();
  }

  public E getNextTypeObject(int ipos)
  {
    return null;
  }

  /** Called by SeqPosition.hasPrevious. */
  protected boolean hasPrevious(int ipos)
  {
    return nextIndex(ipos) != 0;
  }

  /** Return the next position following the argument.
   * The new position has the isAfter property.
   * The argument is implicitly released (as in releasePos).
   * Returns 0 if we are already at end of file.
   */
  public int nextPos (int ipos)
  {
    if (! hasNext(ipos))
      return 0;
    int next = createRelativePos(ipos, 1, true);
    releasePos(ipos);
    return next;
  }

  /** Return the previous position following the argument.
   * The new position has the isBefore property.
   * The argument is implicitly released (as in releasePos).
   * Returns -1 if we are already at beginning of file.
   */
  public int previousPos (int ipos)
  {
    if (! hasPrevious(ipos))
      return 0;
    int next = createRelativePos(ipos, -1, false);
    releasePos(ipos);
    return next;
  }

  /** Set position before first child (of the element following position).
   * @return true if there is a child sequence (which might be empty);
   *   false if current position is end of sequence or following element
   *   is atomic (cannot have children).
   */
  public final boolean gotoChildrenStart(TreePosition pos)
  {
    int ipos = firstChildPos(pos.getPos());
    if (ipos == 0)
      return false;
    pos.push(this, ipos);
    return true;
  }

  /** Get position before first child (of the element following position).
   * @param ipos parent position.  It is not released by this method.
   * @return non-zero position cookie if there is a child sequence
   *   (which might be empty);  zero if current position is end of sequence
   *   or following element is atomic (cannot have children).
   */
  public int firstChildPos (int ipos)
  {
    return 0;
  }

  public int firstChildPos (int ipos, ItemPredicate predicate)
  {
    int child = firstChildPos(ipos);
    if (child == 0)
      return 0;
    if (predicate.isInstancePos(this, child))
      return child;
    else
      return nextMatching(child, predicate, endPos(), false);
  }

  /** Like firstChildPos.
   * Problem: Should this stop before we get to children?
   * I think so, but that requires changes to TreeList. */
  public int firstAttributePos (int ipos)
  {
    return 0;
  }

  /** Get position of parent.
   * @param ipos child position.  It is not released by this method.
   * @return the p os of the parent, or endPos() is there is no known parent.
   */
  public int parentPos (int ipos)
  {
    return endPos();
  }

  protected boolean gotoParent(TreePosition pos)
  {
    if (pos.depth < 0)
      return false;
    pos.pop();
    return true;
  }

  public int getAttributeLength()
  {
    return 0;
  }

  public Object getAttribute(int index)
  {
    return null;
  }

  protected boolean gotoAttributesStart(TreePosition pos)
  {
    return false;
  }

  /** Get the element following the specified position.
   * @param ipos the specified position.
   * @return the following element, or eofValue if there is none.
   * Called by SeqPosition.getNext.
   * FIXME Should change eof handling so return type can be E.
   */
  public Object getPosNext(int ipos)
  {
    if (! hasNext(ipos))
      return Sequence.eofValue;
    return get(nextIndex(ipos));
  }

  /** Get the element before the specified position.
   * @param ipos the specified position.
   * @return the following element, or eofValue if there is none.
   * FIXME Should change eof handling so return type can be E.
   */
  public Object getPosPrevious(int ipos)
  {
    int index = nextIndex(ipos);
    if (index <= 0)
      return Sequence.eofValue;
    return get(index - 1);
  }

  protected void setPosNext(int ipos, E value)
  {
    int index = nextIndex(ipos);
    if (index >= size())
      throw new IndexOutOfBoundsException();
    set(index, value);
  }

  protected void setPosPrevious(int ipos, E value)
  {
    int index = nextIndex(ipos);
    if (index == 0)
      throw new IndexOutOfBoundsException();
    set(index - 1, value);
  }

  public final int nextIndex(SeqPosition pos)
  {
    return nextIndex(pos.ipos);
  }

  /** Compare two positions, and indicate if they are the same position. */
  public boolean equals(int ipos1, int ipos2)
  {
    return compare(ipos1, ipos2) == 0;
  }

  /** Compare two positions, and indicate their relative order. */
  public int compare(int ipos1, int ipos2)
  {
    int i1 = nextIndex(ipos1);
    int i2 = nextIndex(ipos2);
    return i1 < i2 ? -1 : i1 > i2 ? 1 : 0;
  }

  public final int compare(SeqPosition i1, SeqPosition i2)
  {
    return compare(i1.ipos, i2.ipos);
  }

  public Object[] toArray() 
  { 
    int len = size(); 
    Object[] arr = new Object[len];
    int it = startPos();
    int i = 0;
    while ((it = nextPos(it)) != 0)
      arr[i++] = getPosPrevious(it);
    return arr;
  } 

  public <T> T[] toArray(T[] arr) 
  { 
    int alen = arr.length; 
    int len = size(); 
    if (len > alen) 
    { 
      Class componentType = arr.getClass().getComponentType();
      arr = (T[]) java.lang.reflect.Array.newInstance(componentType, len);
      alen = len; 
    }
    
    int it = startPos();
    for (int i = 0;  (it = nextPos(it)) != 0; i++)
    {
      arr[i] = (T) getPosPrevious(it);
    } 
    if (len < alen) 
      arr[len] = null; 
    return arr;
  }

  /** This is used for the XML concept of "document order". */
  public int stableCompare (AbstractSequence other)
  {
    int id1 = System.identityHashCode(this);
    int id2 = System.identityHashCode(other);
    return id1 < id2 ? -1 : id1 > id2 ? 1 : 0;
  }

  /** This is used for the XML concept of "document order".
   * It is overridden in gnu.xml.NodeTree for a more robust implementation.
   */
  public static int compare(AbstractSequence seq1, int pos1,
			    AbstractSequence seq2, int pos2)
  {
    if (seq1 == seq2)
      return seq1.compare(pos1, pos2);
    return seq1.stableCompare(seq2);
  }

  public int hashCode()
  {
    // Implementation specified by the Collections specification.
    int hash = 1;
    for (int i = startPos(); (i = nextPos(i)) != 0;  )
      {
	Object obj = getPosPrevious(i);
	hash = 31*hash + (obj==null ? 0 : obj.hashCode());
      }
    return hash;
  }

  public boolean equals(Object o)
  {
    // Compatible with the Collections specification.
    // FIXME should also depend on class?
    /* #ifdef JAVA2 */
    if (! (this instanceof java.util.List)
        || ! (o instanceof java.util.List))
      return this == o;
    Iterator<E> it1 = iterator();
    Iterator<E> it2 = ((java.util.List<E>) o).iterator();
    /* #endif */
    /* #ifndef JAVA2 */
    // if (! (this instanceof Sequence) || ! (o instanceof Sequence))
    //   return this == o;
    // Enumeration it1 = elements();
    // Enumeration it2 = ((Sequence) o).elements();
    /* #endif */
    for (;;)
      {
        /* #ifdef JAVA2 */
        boolean more1 = it1.hasNext();
        boolean more2 = it2.hasNext();
        /* #endif */
        /* #ifndef JAVA2 */
        // boolean more1 = it1.hasMoreElements();
        // boolean more2 = it2.hasMoreElements();
        /* #endif */
        if (more1 != more2)
          return false;
        if (! more1)
          return true;
	/* #ifdef JAVA2 */
        E e1 = it1.next();
        E e2 = it2.next();
	/* #endif */
	/* #ifndef JAVA2 */
        // Object e1 = it1.nextElement();
        // Object e2 = it2.nextElement();
	/* #endif */
        if (e1 == null)
          {
            if (e2 != null)
              return false;
          }
        else if (! e1.equals(e2))
          return false;
      }
  }

  public Sequence subSequence(SeqPosition start, SeqPosition end)
  {
    return subSequencePos(start.ipos, end.ipos);
  }

  protected Sequence<E> subSequencePos(int ipos0, int ipos1)
  {
    return new SubSequence<E>(this, ipos0, ipos1);
  }

  /* #ifdef JAVA2 */
  public List<E> subList(int fromIx, int toIx)
  {
    return subSequencePos(createPos(fromIx, false),
                          createPos(toIx, true));
  }
  /* #endif */

  /** Copy an element specified by a position pair to a Consumer.
   * @return if hasNext(ipos). */
  public boolean consumeNext (int ipos, Consumer out)
  {
    int next = nextPos(ipos);
    if (next == 0)
      return false;
    consumePosRange(ipos, next, out);
    return true;
  }

  public void consumePosRange(int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int it = copyPos(iposStart);
    while (! equals(it, iposEnd))
      {
	if (! hasNext(it))
          throw new RuntimeException();
	out.writeObject(getPosNext(it));
        it = nextPos(it);
      }
    releasePos(it);
  }
  
    public void consume(int fromIndex, int toIndex, Consumer out) {
        int ipos0 = createPos(fromIndex, false);
        int ipos1 = createPos(toIndex, true);
        consumePosRange(ipos0, ipos1, out);
        releasePos(ipos0);
        releasePos(ipos1);
    }

  public void consume(Consumer out)
  {
    boolean isSequence = this instanceof Sequence;
    if (isSequence)
      out.startElement("#sequence");
    consumePosRange(startPos(), endPos(), out);
    if (isSequence)
      out.endElement();
  }

  public void toString (String sep, StringBuffer sbuf)
  {
    boolean seen = false;
    for (int i = startPos();  (i = nextPos(i)) != 0; )
      {
	if (seen)
	  sbuf.append(sep);
	else
	  seen = true;
	sbuf.append(getPosPrevious(i));
      }
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (this instanceof Sequence)
      sbuf.append('[');
    toString(", ", sbuf);
    if (this instanceof Sequence)
      sbuf.append(']');
    return sbuf.toString();
  }
}
