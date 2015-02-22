// Copyright (c) 2001, 2003, 2005, 2007  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** Editable character sequence using a a buffer-gap implementstion and
 * self-adjusting position.
 * Can implement (the text part of) an Emacs buffer, or a
 * javax.swing.text.AbstractDocument.Content
 */

public class CharBuffer extends StableVector
  implements CharSeq, java.io.Serializable
{
  // Same as super.base but pre-cast to FString.
  private FString string;

  public CharBuffer(FString str)
  {
    super(str);
    string = str;
  }

  public CharBuffer(int initialSize)
  {
    this(new FString(initialSize));
  }

  protected CharBuffer ()
  {
  }

  public int length() { return size(); }

  public char charAt(int index)
  {
    // If index is out of bounds, the base.get will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    return string.charAt(index);
  }

  public int indexOf (int ch, int fromChar)
  {
    char c1, c2;
    if (ch >= 0x10000)
      {
        c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
        c2 = (char) ((ch & 0x3FF) + 0xDC00);
      }
    else
      {
        c1 = (char) ch;
        c2 = 0;
      }
    char[] arr = getArray();
    int i = fromChar;
    int limit = gapStart;
    if (i >= limit)
      {
        i += gapEnd-gapStart;
        limit = arr.length;
      }
    for ( ; ; i++)
      {
        if (i == limit)
          {
            limit = arr.length;
            if (i < limit)
              i = gapEnd;
            else
              break;
          }
        if (arr[i] == c1
            && (c2 == 0
                || (i+1 < limit ? arr[i+1] == c2
                    : gapEnd < arr.length && arr[gapEnd] == c2)))
          return i > gapStart ? i - (gapEnd - gapStart) : i;
      }
    return -1;
  }

  public int lastIndexOf (int ch, int fromChar)
  {
    char c1, c2;
    if (ch >= 0x10000)
      {
        c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
        c2 = (char) ((ch & 0x3FF) + 0xDC00);
      }
    else
      {
        c1 = 0;
        c2 = (char) ch;
      }
    for (int i = fromChar; --i >= 0; )
      {
        if (charAt(i) == c2)
          {
            if (c1 == 0)
              return i;
            if (i > 0 && charAt(i-1) == c1)
              return i - 1;
          }
      }
    return -1;
  }

  /** Copy characters into a destination buffer.
   * Same interface as java.lang.String's getChars. */
  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    char[] array = string.data;
    int count;
    if (srcBegin < gapStart)
      {
	count = (srcEnd < gapStart ? srcEnd : gapStart) - srcBegin;
	if (count > 0)
	  {
	    System.arraycopy(array, srcBegin, dst, dstBegin, count);
	    srcBegin += count;
	    dstBegin += count;
	  }
      }
    int gapSize = gapEnd - gapStart;
    srcBegin += gapSize;
    srcEnd += gapSize;
    count = srcEnd - srcBegin;
    if (count > 0)
      System.arraycopy(array, srcBegin, dst, dstBegin, count);
  }

  public void setCharAt(int index, char value)
  {
    // If index is out of bounds, the base.get will catch that.
    if (index >= gapStart)
      index += gapEnd - gapStart;
    string.setCharAt(index, value);
  }

    public void setCharacterAt(int index, int ch) {
        int sz = size();
        if (index < 0 || index >= sz)
            throw new StringIndexOutOfBoundsException(index);
        char old1 = charAt(index);
        char old2;
        boolean oldIsSupp = old1 >= 0xD800 && old1 <= 0xDBFF
            && index+1 < sz
            && (old2 = charAt(index+1)) >= 0xDC00 && old2 <= 0xDFFF;
        if (ch <= 0xFFFF) {
            if (oldIsSupp)
                delete(index+1, 1);
            setCharAt(index, (char) ch);
        } else {
            char c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
            char c2 = (char) ((ch & 0x3FF) + 0xDC00);
            setCharAt(index, c1);
            if (oldIsSupp) {
                setCharAt(index+1, c2);
            } else {
                // Optimization of:
                // insert(index+1, new String(new char[] { c2 }), true);
                gapReserve(index+1, 1);
                string.setCharAt(index+1, c2);
                gapStart += 1;
                // Any position after old single-wide char are now in the
                // middle of the new char pair.  Adjust to be after c2.
                int oldPos = (gapStart-1)<<1;
                adjustPositions(oldPos, oldPos + 1, 2);
            }
        }
    }


  public String substring (int start, int end)
  {
    int sz = size();
    if (start < 0 || end < start || end > sz)
      throw new IndexOutOfBoundsException();
    int len = end - start;
    start = getSegment(start, len);
    return new String(getArray(), start, len);
  }
  

  /* #ifdef use:java.lang.CharSequence */
  public CharSequence subSequence(int start, int end)
  {
    int sz = size();
    if (start < 0 || end < start || end > sz)
      throw new IndexOutOfBoundsException();
    return new SubCharSeq(this,
                          base.createPos(start, false),
                          base.createPos(end, true));
  }
  /* #endif */

  public void fill(int fromIndex, int toIndex, char value)
  {
    char[] array = string.data;
    int i = fromIndex;
    int limit = gapStart < toIndex ? gapStart : toIndex;
    for (;  i < limit;  i++)
      array[i] = value;
    int gapSize = gapEnd - gapStart;
    i = limit + gapSize;
    limit += toIndex;
    for (;  i < limit;  i++)
      array[i] = value;
  }

  /** Set all the elements to a given character. */
  public final void fill (char value)
  {
    char[] array = string.data;
    for (int i = array.length;  --i >= gapEnd; )
      array[i] = value;
    for (int i = gapStart;  --i >= 0; )
      array[i] = value;
  }

  public char[] getArray() { return (char[]) base.getBuffer(); }

  public void delete(int where, int count)
  {
    int ipos = createPos(where, false);
    removePos(ipos, count);
    releasePos(ipos);
  }

  public void insert(int where, String str, boolean beforeMarkers)
  {
    int len = str.length();
    gapReserve(where, len);
    str.getChars(0, len, string.data, where);
    gapStart += len;
    if (beforeMarkers) {
        // Adjust markers at insertion point to be after inserted next.
        int oldPos = (gapStart-len) << 1;
        adjustPositions(oldPos, oldPos + 1, len << 1);
    }
  }

  public void consume(int start, int count, Consumer dest)
  {
    char[] array = string.data;
    if (start < gapStart)
      {
	int count0 = gapStart - start;
	if (count0 > count)
	  count0 = count;
	dest.write(array, start, count0);
	count -= count0;
	start += count;
      }
    if (count > 0)
      {
	start += gapEnd - gapStart;
	dest.write(array, start, count);
      }
  }

  public String toString()
  {
    int len = size();
    int start = getSegment(0, len);
    return new String(getArray(), start, len);
  }

  /* #ifdef JAVA5 */
  public void writeTo(int start, int count, Appendable dest)
    throws java.io.IOException
  {
    if (dest instanceof java.io.Writer)
      writeTo(start, count, (java.io.Writer) dest);
    else
      dest.append(this, start, start+count);
  }

  public void writeTo(Appendable dest)
    throws java.io.IOException
  {
    writeTo(0, size(), dest);
  }
  /* #endif */

  public void writeTo(int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    char[] array = string.data;
    if (start < gapStart)
      {
	int count0 = gapStart - start;
	if (count0 > count)
	  count0 = count;
	dest.write(array, start, count0);
	count -= count0;
	start += count;
      }
    if (count > 0)
      {
	start += gapEnd - gapStart;
	dest.write(array, start, count);
      }
  }

  public void writeTo(java.io.Writer dest) throws java.io.IOException
  {
    char[] array = string.data;
    dest.write(array, 0, gapStart);
    dest.write(array, gapEnd, array.length - gapEnd);
  }

  public void dump()
  {
    System.err.println("Buffer Content dump.  size:"+size()+"  buffer:"+getArray().length);
    System.err.print("before gap: \"");
    System.err.print(new String(getArray(), 0, gapStart));
    System.err.println("\" (gapStart:"+gapStart+" gapEnd:"+gapEnd+')');
    System.err.print("after gap: \"");
    System.err.print(new String(getArray(), gapEnd, getArray().length-gapEnd));
    System.err.println("\"");
    int poslen = positions == null ? 0 : positions.length;
    System.err.println("Positions (size: "+poslen+" free:"+free+"):");
    boolean[] isFree = null;
    if (free != -2)
      {
        isFree = new boolean[positions.length];
        for (int i = free;  i >= 0;  i = positions[i])
          isFree[i] = true;
      }
    for (int i = 0;  i < poslen;  i++)
      {
	int pos = positions[i];
	if (free == -2 ? pos != FREE_POSITION : ! isFree[i]) {
            int p = pos>>1;
            if (p > gapStart)
                p -= gapEnd-gapStart;
	  System.err.println("position#"+i+": "+p+" isAfter:"+(pos&1));
        }
      }
  }
}
