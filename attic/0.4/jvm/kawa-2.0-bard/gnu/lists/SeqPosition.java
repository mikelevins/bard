// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.
package gnu.lists;

import java.util.NoSuchElementException;

/**
 * A position in a sequence (list).
 *
 * Conceptually similar to Java2's ListIterator, but we use the name "Position"
 * to indicate that it can be used to both indicate a position in a sequence
 * and to iterate through a sequence.  If you use a SeqPosition as a
 * "position", you would not modify if (though it is possible the offset
 * of the position in the sequence may change due to other update operations
 * on the sequence).  If you use a SeqPosition as an "iterator", you would
 * initialize it to some beginnning position, and then modify the current
 * position of the SeqPosition so it refers to successive elements.
 *
 * See the <a href="package-summary.html#iteration">package overview</a>
 * for more information.
 */

public class SeqPosition<E, ESEQ extends AbstractSequence<E>>
implements
    /* #ifdef JAVA2 */
    java.util.ListIterator<E>,
    /* #endif */
    java.util.Enumeration<E>
// Maybe implement Spliterator?
{
  /**
   * The Sequence relative to which ipos and xpos have meaning.
   * This is normally the same as the Sequence we iterate through.
   * However, if this is a TreePosition, it may an ancestor instead.
   */
  public ESEQ sequence;

  /**
   * An integer that (together with xpos) indicates the current position.
   * The actual value has no meaning, except as interpreted by sequence.
   */
  public int ipos;

  public SeqPosition()
  {
  }

  public SeqPosition(ESEQ seq)
  {
    this.sequence = seq;
  }

  public SeqPosition(ESEQ seq, int offset, boolean isAfter)
  {
    this.sequence = seq;
    ipos = seq.createPos(offset, isAfter);
  }

  public SeqPosition(ESEQ seq, int ipos)
  {
    this.sequence = seq;
    this.ipos = ipos;
  }

  /** Creates a new SeqPosition, from a position pair.
   * The position pair is copied (using copyPos).
   */
  public static <E,ESEQ extends AbstractSequence<E>> SeqPosition<E,ESEQ> make(ESEQ seq, int ipos)
  {
    return new SeqPosition<E,ESEQ>(seq, seq.copyPos(ipos));
  }

  public SeqPosition<E,ESEQ> copy ()
  {
    return new SeqPosition<E,ESEQ>(sequence, sequence.copyPos(getPos()));
  }
 
  public final void gotoStart(ESEQ seq)
  {
    setPos(seq, seq.startPos());
  }

  public final void gotoEnd(ESEQ seq)
  {
    setPos(seq, seq.endPos());
  }

  /** Set position before first child (of the element following position).
   * @return true if there is a child sequence (which might be empty);
   *   false if current position is end of sequence or following element
   *   is atomic (cannot have children).
   */
  public boolean gotoChildrenStart()
  {
    int child = sequence.firstChildPos(getPos());
    if (child == 0)
      return false;
    ipos = child;
    return true;
  }

  /** True if there is an element following the current position.
   * False if we are at the end.  See java.util.Enumeration. */
  public final boolean hasMoreElements()
  {
    return hasNext();
  }

  /** See java.util.Iterator. */
  public boolean hasNext()
  {
    return sequence.hasNext(getPos());
  }

    // FUTURE: /* #ifdef JAVA8 */
    // public boolean tryAdvance(java.util.function.Consumer<? super E> action) {
    //     if (hasNext()) {
    //         action.accept(next());
    //         return true;
    //     }
    //     return false;
    // }
    // public int characteristics() {
    //     /* Should call sequence.spliterator$characteristics(); */
    //     return ORDERED | SIZED | SUBSIZED;
    // }
    // /* #endif */

  /** Return a code (defined in Sequence) for the type of the next element. */
  public int getNextKind()
  {
    return sequence.getNextKind(getPos());
  }

  /** Get the "tag name" for the next element, if any. */
  public String getNextTypeName()
  {
    return sequence.getNextTypeName(getPos());
  }

  /** Get the "tag object" for the next element, if any. */
  public E getNextTypeObject()
  {
    return sequence.getNextTypeObject(getPos());
  }

  /** See java.util.Iterator. */
  public boolean hasPrevious()
  {
    return sequence.hasPrevious(getPos());
  }

  /** See java.util.ListIterator. */
  public E next()
  {
    Object result = getNext();
    if (result == Sequence.eofValue || ! gotoNext())
      throw new NoSuchElementException();
    return (E) result;
  }

  /** Move one element forwards, if possible.
   * @return if we succeeded in moving forwards (i.e. not at end of sequence).
   */
  public boolean gotoNext()
  {
    int next = sequence.nextPos(ipos);
    if (next != 0)
      {
	ipos = next;
	return true;
      }
    else
      {
	ipos = -1;
	return false;
      }
  }

  /** Move backwards one element.
   * @return false iff already at beginning.
   */
  public boolean gotoPrevious()
  {
    int prev = sequence.previousPos(ipos);
    if (prev != -1)
      {
	ipos = prev;
	return true;
      }
    else
      {
	ipos = 0;
	return false;
      }
  }

  /** See java.util.ListIterator. */
  @SuppressWarnings("unchecked")
  public E previous()
  {
    Object result = getPrevious();
    if (result == Sequence.eofValue || ! gotoPrevious())
      throw new NoSuchElementException();
    return (E) result;
  }

  /** See java.util.Enumeration. */
  public final E nextElement()
  {
    return next();
  }

  /**
   * Get element following current position.
   * Does not move the position, in contrast to next() method.
   * @return EOF if at end of sequence, otherwise the value following.
   */
  public Object getNext()
  {
    return sequence.getPosNext(getPos());
  }

  /**
   * Get element before the current position.
   * Does not move the position, in contrast to previous() method.
   * @return EOF if at beginning of sequence, otherwise the value prior.
   */
  public Object getPrevious()
  {
    return sequence.getPosPrevious(getPos());
  }

  /** See java.util.Iterator. */
  public int nextIndex()
  {
    return sequence.nextIndex(getPos());
  }

  public final int fromEndIndex()
  {
    return sequence.fromEndIndex(getPos());
  }

  public int getContainingSequenceSize()
  {
    return sequence.getContainingSequenceSize(getPos());
  }

  /** See java.util.Iterator. */
  public final int previousIndex()
  {
    return sequence.nextIndex(getPos()) - 1;
  }

  /** Tests whether the position pair has the "isAfter" property.
   * I.e. if something is inserted at the position, will
   * the iterator end up being after the new data?
   * A toNext() or next() command should set isAfter() to true;
   * a toPrevious or previous command should set isAfter() to false.
   */
  public boolean isAfter()
  {
    return sequence.isAfterPos(getPos());
  }

  public final void set(E value)
  {
    if (isAfter())
      setPrevious(value);
    else
      setNext(value);
  }

  public void setNext (E value)
  {
    sequence.setPosNext(getPos(), value);
  }

  public void setPrevious (E value)
  {
    sequence.setPosPrevious(getPos(), value);
  }

  public void remove()
  {
    sequence.removePos(getPos(), isAfter() ? -1 : 1);
  }

  public void add(E o)
  { 
    setPos(sequence.addPos(getPos(), o));
  }

  /** Get a position int "cookie" for this SeqPosition.
   * The result can be passed to AbstractSequence's getPosNext(int),
   * createRelativePos, and other methods.
   * By default this is the value of ipos, but for sequences that need emore
   * state than an ipos for efficient position, we use a PositionManager index.
   * So this gets over-ridden in ExtPosition.
   */
  public int getPos ()
  {
    return ipos;
  }

  public void setPos (ESEQ seq, int ipos)
  {
    if (sequence != null)
      sequence.releasePos(getPos());
    this.ipos = ipos;
    this.sequence = seq;
  }

  public void setPos (int ipos)
  {
    if (sequence != null)
      sequence.releasePos(getPos());
    this.ipos = ipos;
  }

  public void set (ESEQ seq, int index, boolean isAfter)
  {
    if (sequence != null)
      sequence.releasePos(ipos);
    sequence = seq;
    ipos = seq.createPos(index, isAfter);
  }

  public void set (SeqPosition<E,ESEQ> pos)
  {
    if (sequence != null)
      sequence.releasePos(ipos);
    sequence = pos.sequence;
    pos.ipos = sequence.copyPos(pos.ipos);
  }

  public void release()
  {
    if (sequence != null)
      {
	sequence.releasePos(getPos());
	sequence = null;
      }
  }

  public void finalize()
  {
    release();
  }

  public String toString()
  {
    if (sequence == null)
      return toInfo();
    Object item = sequence.getPosNext(ipos);
    return "@"+nextIndex()+": "+(item==null ? "(null)" : item.toString());
  }

  public String toInfo()
  {
    StringBuffer sbuf = new StringBuffer(60);
    sbuf.append('{');
    if (sequence == null)
      sbuf.append("null sequence");
    else
      {
	sbuf.append(sequence.getClass().getName());
	sbuf.append('@');
	sbuf.append(System.identityHashCode(sequence));
      }
    sbuf.append(" ipos: ");
    sbuf.append(ipos);
    /*
    if (sequence instanceof TreeList)
      {
	sbuf.append(" index: ");
	sbuf.append(((TreeList) sequence).posToDataIndex(ipos));
      }
    */
    sbuf.append('}');
    return sbuf.toString();
  }
}
