// Copyright (c) 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

public class PositionManager
{
  static final PositionManager manager = new PositionManager();

  static public SeqPosition getPositionObject (int ipos)
  {
    PositionManager m = manager;
    synchronized (m)
      {
	return m.positions[ipos];
      }
  }

  SeqPosition[] positions = new SeqPosition[50];
  int[] ivals = new int[50];
  int freeListHead = -1;

  private void addToFreeList(int[] ivals, int first, int end)
  {
    int head = freeListHead;
    for (int i = first; i < end;  i++)
      {
	ivals[i] = head;
	head = i;
      }
    freeListHead = head;
  }

  private int getFreeSlot ()
  {
    int head = freeListHead;
    if (head < 0)
      {
	int old_size = positions.length;
	SeqPosition[] npositions = new SeqPosition[2 * old_size];
	int[] nvals = new int[2 * old_size];
	System.arraycopy(positions, 0, npositions, 0, old_size);
	System.arraycopy(ivals, 0, nvals, 0, old_size);
	positions = npositions;
	ivals = nvals;
	addToFreeList(nvals, old_size, 2 * old_size);
	head = freeListHead;
      }
    freeListHead = ivals[head];
    return head;
  }

  public PositionManager ()
  {
    // We don't use positions[0], because ipos==0 is reserved for
    // createPosition(0, false).
    addToFreeList(ivals, 1, ivals.length);
  }

  public synchronized int register(SeqPosition pos)
  {
    int i = getFreeSlot();
    positions[i] = pos;
    ivals[i] = -1;
    return i;
  }

  public synchronized void release(int ipos)
  {
    SeqPosition pos = positions[ipos];
    if (pos instanceof ExtPosition)
      ((ExtPosition) pos).position = -1;
    positions[ipos] = null;
    ivals[ipos] = freeListHead;
    freeListHead = ipos;
    pos.release();
  }
}
