package gnu.kawa.util;
import java.util.Hashtable;

/** Map integers to Object.
 * Future implementaton will be optimized for ranges that map to the
 * same value, but the current implementation is bad except for 0..127. */

public class RangeTable implements Cloneable // extends map
{
  Object[] index = new Object[128];
  Hashtable hash = new Hashtable(200);

  public Object lookup(int key, Object defaultValue)
  {
    if ((key & 127) == key)
      return index[key];
    return hash.get(new Integer(key));
  }

  public void set(int lo, int hi, Object value)
  {
    if (lo > hi)
      return;
    for (int i = lo;  ; i++)
      {
	if ((i & 127) == i)
	  index[i] = value;
	else
	  hash.put(new Integer(i), value);
	if (i == hi)
	  break;
      }
  }

  public void set(int key, Object value)
  {
    set(key, key, value);
  }

  public void remove(int lo, int hi)
  {
    if (lo > hi)
      return;
    for (int i = lo;  ; i++)
      {
	if ((i & 127) == i)
	  index[i] = null;
	else
	  hash.remove(new Integer(i));
	if (i == hi)
	  break;
      }
  }

  public void remove(int key)
  {
    remove(key, key);
  }

  public RangeTable copy()
  {
    RangeTable copy = new RangeTable();
    copy.index = (Object[]) index.clone();
    copy.hash = (Hashtable) hash.clone();
    return copy;
  }

  public Object clone()
  {
    return copy();
  }

  /*
  Object[] index = null;
  //int index256 = null;

  Range ranges;
  
  private static void update(Object[] index, int lo, int hi, Object value)
  {
  }

  private Object update(int lo, int hi, Object value, int shift, Object[] index)
  {
    if (index == null)
      {
	if (shift == 24)
	  index = new Range(lo, hi, value);
	else
	  index = new Object[16];
      }
    int ilo = (lo >> shift) & 0xf;
    int ihi = (hi >> shift) & 0xf;
    for (int i = ilo;  i < ihi;  i++)
      {
	index[i] = update(lo, hi, shift - 4, index[i]);
      }
  }

  public void set(int lo, int hi, Object value)
  {
    set(lo, ho, value, index, 24);
  }

  private void set(int lo, int hi, Object value, Object[] index, int shift)
  {
    Range r = findLower(lo);
    if (r == null)
      {
	r = new Range(lo, hi, value, index);
	// ???
      }
    // Now: r != null && r.lo <= lo && (r.next == null || r.next.lo > lo).
    Range next = r.next;
    if (lo <= r.hi)
      {
	// Overlap: lo is within r.

	if (lo == r.lo || hi == r.hi)
	  {
	    r.value = value;
	    return;
	  }
	if (hi <= r.hi)
	  {
	    // Completely within r.
	    if (r.value == value)
	      return;
	    Range s = new Range(lo, hi, value, next);
	    if (hi != r.hi)
	      s.next = t = new Range(hi + 1, r.hi, r.value, next);
	    r.hi = lo - 1;
	    r.next = s;
	    // goto fixup;
	  }
      }
    else if (lo == r.hi + 1 && value == r.value)
      {
	r.hi = hi;
	if (next != null)
	  {
	    if (value == next.value && hi + 1 >= next.lo)
	      {
		r.next = next.next;
		r.hi = next.hi;
	      }
	    while (hi >= next.hi)
	      {
		r.hi = hi = next.hi;
		next = next.next;
		r.next = next;
	      }
	    if (hi >= next.lo)
	      {
		next.lo ...
	      }
	  }
      }
  }

  private static Range getLast(Object[] index, int key)
  {
    for (;;)
      {
	Object cur = index[key];
	if (cur == null)
	  {
	    if (key == 0)
	      return null;
	    key--;
	    continue;
	  }
	if (cur instanceof Range)
	  return (Range) cur;
	key = 15;
	index = (Object[]) cur;
      }
  }
  */

  /* Find last range r such that r.hi <= key. */
  /*
  public Range findLower(int key)
  {
    Object cur;
    int shift;
    //if ((key & 0xFF) == key)
    //  {
    //    // Optimize for key in range [0 .. 256].
    //    cur = index256;
    //    shift = 4;
    //  }
    //  else
      {
	cur = index;
	shift = 24;
	// FIXME - handle negative values in proper order?
	// key ^= 0x8000000;
      }
    for (;;)
      {
	int key15 = (key >> shift) & 0xF;
	cur = index[key15];
	if (cur == null)
	  return getLast(index, key15);
	if (cur instanceof Range)
	  return (Range) cur;
	shift -= 24;
      }

  }

  public Object lookup(int key, Object defaultValue)
  {
    Object cur;
    int shift;
    if ((key & 0xFF) == key)
      {
	// Optimize for key in range [0 .. 256].
	cur = index256;
	shift = 4;
      }
    else
      {
	cur = index;
	shift = 24;
	// FIXME - handle negative values in proper order?
	// key ^= 0x8000000;
      }
    for (;;)
      {
	cur = index[(key >> shift) & 0xF];
	if (cur == null)
	  return defaultValue;
	if (cur instanceof Range)
	  {
	    Range range = (Range) cur;
	    if (key <= range.lo && key >= range.hi)
	      return range.value;
	    else
	      return defaultValue;
	  }
	shift -= 24;
      }


  }

  public Object enumerate (RangeHandler handler)
  {
    for (Range range;  range != null; range = range.chain)
      {
	handel.handle(range.lo, range.hi, range.value);
      }
  }
  */
}

/*
interface RangeHandler
{
  public Object handle(int lo, int hi, Object value);
}

class Range
{
  int lo;
  int hi;
  Object value;
  Range next;

  public Range(int lo, int hi, Object value, Range next)
  {
    this.lo = lo;
    this.hi = hi;
    this.value = value;
    this.next = next;
  }
}
*/
