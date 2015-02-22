// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** LListPosition - a SeqPosition for LLists.
 * Normally, we just use an int pos to indicate a position in a
 * sequence.  But for linked lists that would be very inefficient.
 * So this sub-class in addition uses an xpos field to point to the
 * current Pair.  However, we do so in a non-obvious way.  After a next(),
 * xpos points to *two* Pairs before the logical iterator position
 * (the cursor).  This is because of the requirement for remove(),
 * which needs to be able to remove the element *before* the cursor.
 * But to unlink that element, we need to change the 'next' field
 * of the Pair before that again.
 * However, after a call to 'previous', we cannot cheaply move the
 * xpos pointer backwards, so we leave it as is.  Therefore, we
 * get the rule that when isAfter() is false the cursor position is
 * after the Pair pointed by xpos; when isAfter() is true then
 * the cursor position is after the Pair ((Pair) xpos).cdr).
 * If the cursor position is too early in the list to satisfy this
 * invariant, then xpos==null.
 */

class LListPosition extends ExtPosition<Object, LList>
{
  Object xpos;

  public LListPosition (LListPosition old)
  {
    sequence = old.sequence;
    ipos = old.ipos;
    xpos = old.xpos;
  }

  public LListPosition copy()
  {
    return new LListPosition(this);
  }

  public LListPosition (LList seq, int index, boolean isAfter)
  {
    set(seq, index, isAfter);
  }

  public void set (LList seq, int index, boolean isAfter)
  {
    sequence = seq;
    ipos = (index << 1) | (isAfter ? 1 : 0);
    int skip = index;
    if (isAfter)
      {
        skip -= 2;
      }
    else
      {
        skip -= 1;
      }
    if (skip >= 0)
      {
	Object p = seq;
        while (--skip >= 0)
          {
            p = ((Pair) p).cdr;
          }
	xpos = p;
      }
    else
      xpos = null;
  }

  public boolean hasNext()
  {
    Object next;
    if (xpos == null)
      {
        if ((ipos >> 1) == 0)
          return sequence != LList.Empty;
        return ((Pair) sequence).cdr != LList.Empty;
      }
    else
      next = ((Pair) xpos).cdr;
    if ((ipos & 1) > 0) // if isAfter
      next = ((Pair) next).cdr;
    return next != LList.Empty;
  }

  public boolean hasPrevious()
  {
    return (ipos >>> 1) != 0;
  }

  /** Get the Pair whose car contains the next element, or null. */
  public Pair getNextPair ()
  {
    int isAfter = (ipos & 1);
    Object next;
    if (isAfter > 0)
      {
        if (xpos == null)
          {
	    next = sequence;
	    if ((ipos >> 1) != 0)
	      next = ((Pair) next).cdr;
          }
        else
          next = ((Pair) (((Pair) xpos).cdr)).cdr;
      }
    else
      {
        if (xpos == null)
          next = sequence;
        else
          next = ((Pair) xpos).cdr;
      }
    if (next == LList.Empty)
      return null;
    return (Pair) next;
  }

  public Object getNext ()
  {
    Pair pair = getNextPair();
    return pair == null ? LList.eofValue : pair.car;
  }

  public void setNext (Object value)
  {
    Pair pair = getNextPair();
    pair.car = value;
  }

  /** Get the Pair whose car contains the previous element, or null. */
  public Pair getPreviousPair ()
  {
    int isAfter = (ipos & 1);
    Object p = xpos;
    if (isAfter > 0)
      p = p == null ? sequence : ((Pair) p).cdr;
    else if (p == null)
      return null;
    if (p == LList.Empty)
      return null;
    return (Pair) p;
  }

  public Object getPrevious ()
  {
    Pair pair = getPreviousPair();
    return pair == null ? LList.eofValue : pair.car;
  }

  public void setPrevious(Object value)
  {
    Pair pair = getPreviousPair();
    pair.car = value;
  }

  public int nextIndex ()
  {
    return ipos >> 1;
  }

  public boolean gotoNext()
  {
    boolean isAfter = (ipos & 1) != 0;
    int old_i = ipos;
    Object xp = xpos;
    if (xp != null)
      {
	if (isAfter)
	  xp = ((Pair) xp).cdr;
	if (((Pair) xp).cdr == LList.Empty)
	  return false;
	xpos = xp;
	ipos = (ipos | 1) + 2;
      }
    else if ((ipos >> 1) == 0) // position is 0
      {
	if (sequence == LList.Empty)
	  return false;
	ipos = (1 << 1) | 1;
      }
    else // position is 1, iAfter must be true
      {
	xp = sequence;
	if (((Pair) xp).cdr == LList.Empty)
	  return false;
	ipos = (2 << 1) | 1;
	xpos = xp;
      }
    return true;
  }

  public boolean gotoPrevious()
  {
    if (ipos >>> 1 == 0)
      return false;
    if ((ipos & 1) != 0) // isAfter()
      {
	// Decrement index; clear isAfter bit.
	ipos -= 3;
	return true;
      }
    int index = nextIndex();
    set((LList) sequence, index - 1, false);
    return true;
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("LListPos[");
    //sbuf.append(super.toString());
    sbuf.append("index:");
    sbuf.append(ipos);
    if (isAfter())
      sbuf.append(" after");
    if (position >= 0)
      {
	sbuf.append(" position:");
	sbuf.append(position);
      }
    sbuf.append(']');
    return sbuf.toString();
  }
}
