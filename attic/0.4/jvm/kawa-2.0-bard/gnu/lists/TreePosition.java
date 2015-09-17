// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/**
 * A position that can also go down and up in a tree. 
 * A TreePosition is a stack of positions.  The "current" position
 * (i.e. the one you get if you tree the TreePosition as a SeqPosition)
 * is that in the innermost containing sequence.
 *
 * Normally, the "current" element is (the one following) a position in a
 * sequence.  As a special (initial case), we may want to treat the
 * entire sequence is the "current element".  This is represented by depth==-1
 * and xpos set to the root element (which need not actually be a sequence).
 */

public class TreePosition extends SeqPosition implements Cloneable
{
  /** Used when depth==-1 to indicate the "entire" object.
   * Usually an AbstractSequence, but need not be. */
  private Object xpos;

  /** Stack of pushed values for sequence. */
  AbstractSequence[] sstack;

  /** Stack of pushed values for iposition. */
  int[] istack; 

  /** Depth of the above stacks.
   * Note that getDepth returns depth+1; this should perhaps match. */
  int depth;

  /** Start of stacks - anything below 'start' is ignored.
   * This is useful for pushing/pop positions without object allocation. */
  int start;

  public TreePosition()
  {
    depth = -1;
  }

  /** Not a position *in* a sequence, but the current element is the entire sequence. */
  public TreePosition(Object root)
  {
    xpos = root;
    depth = -1;
  }

  public TreePosition(AbstractSequence seq, int index)
  {
    super(seq, index, false);
  }

  public TreePosition (TreePosition pos)
  {
    set(pos);
  }

  public Object clone ()
  {
    return new TreePosition(this);
  }

  public void set (TreePosition position)
  {
    release();
    int d = position.depth;
    depth = d;
    if (d < 0)
      {
	xpos = position.xpos;
	return;
      }
    if (sstack == null || sstack.length <= d)
      sstack = new AbstractSequence[d + 10];
    if (istack == null || istack.length <= d)
      istack = new int[d + 10];
    AbstractSequence seq;
    int i;
    for (i = 0;  i < depth;  i++)
      {
	int j = i + position.start;
	seq = position.sstack[j];
	sstack[depth-1] = seq;
	istack[depth - i] = seq.copyPos(position.istack[j]);
      }
    seq = position.sequence;
    sequence = seq;
    ipos = seq.copyPos(position.ipos);
  }

  /** Number of ancestor sequences, including current sequence. */
  public int getDepth()
  {
    return depth + 1;
  }

  /** Get the "root document". */
  public AbstractSequence getRoot()
  {
    return depth == 0 ? sequence : sstack[start];
  }

  public Object getPosNext()
  {
    return sequence == null ? xpos : sequence.getPosNext(ipos);
  }

  public void push(AbstractSequence child, int iposChild)
  {
    int d = depth + start;
    if (d >= 0)
      {
	if (d == 0)
	  {
	    istack = new int[8];
	    sstack = new AbstractSequence[8];
	  }
	else if (d >= istack.length)
	  {
	    int ndepth = 2 * d;
	    int[] itemp = new int[ndepth];
	    Object[] xtemp = new Object[ndepth];
	    AbstractSequence[] stemp = new AbstractSequence[ndepth];
	    System.arraycopy(istack, 0, itemp, 0, depth);
	    System.arraycopy(sstack, 0, stemp, 0, depth);
	    istack = itemp;
	    sstack = stemp;
	  }
	sstack[d] = sequence;
	istack[d] = ipos;
      }
    depth++;
    sequence = child;
    ipos = iposChild;
  }

  public void pop()
  {
    sequence.releasePos(ipos);
    popNoRelease();
  }

  public void popNoRelease()
  {
    if (--depth < 0)
      {
	xpos = sequence;
	sequence = null;
      }
    else
      {
	sequence = sstack[start+depth];
	ipos = istack[start+depth];
      }
  }

  public final boolean gotoParent()
  {
    return sequence == null ? false : sequence.gotoParent(this);
  }

  /** Set position before first child (of the element following position).
   * @return true if there is a child sequence (which might be empty);
   *   false if current position is end of sequence or following element
   *   is atomic (cannot have children).
   */
  public boolean gotoChildrenStart()
  {
    if (sequence == null)
      {
	if (! (xpos instanceof AbstractSequence))
	  return false;
	depth = 0;
	sequence = (AbstractSequence) xpos;
	setPos(sequence.startPos());
      }
    else
      {
	if (! sequence.gotoChildrenStart(this))
	  return false;
      }
    return true;
  }

  /** Set position before first attribute (of the element following position).
   * This is used to iterate through the sequence of attributes.
   */
  public boolean gotoAttributesStart()
  {
    if (sequence == null)
      {
	if (xpos instanceof AbstractSequence)
	  {
	    // ??? FIXME
	  }
	return false;
      }
    return sequence.gotoAttributesStart(this);
  }

  /*
  public boolean gotoAttribute(Object name)
  {
    return sequence.gotoAttribute(this);
  }
  */

  /** Get the value of an ancestor node.
   * @param up the number parents to go up.
   * @return if up is 0, same getNext.   Otherwise get parent
   * applied as specified.
   */
  public Object getAncestor(int up)
  {
    if (up == 0)
      return sequence.getPosNext(ipos);
    int i = depth - up;
    if (i <= 0)
      return getRoot();
    i += start;
    return sstack[i].getPosNext(istack[i]);
  }

  public void release()
  {
    while (sequence != null)
      {
        sequence.releasePos(ipos);
        pop();
      }
    xpos = null;
  }

  /** Copy this position into pos. */
  /*
  public void clone (Position pos)
  {
    // FIXME!
  }

  public Object clone()
  {
    TreePosition pos = new TreePosition();
    clone(pos);
    return pos;
  }
  */

  public void dump()
  {
    System.err.println("TreePosition dump depth:"+depth+" start:"+start);
    for (int i = 0;  i <= depth;  i++)
      {
	AbstractSequence seq = i==0 ? sequence : sstack[depth-i];
	System.err.print("#"+i+" seq:"+seq);
	System.err.println(" ipos:" + (i == 0 ? ipos : istack[depth-i]));
      }
  }
}
