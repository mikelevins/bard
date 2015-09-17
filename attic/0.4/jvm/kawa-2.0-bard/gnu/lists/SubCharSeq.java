package gnu.lists;

public class SubCharSeq extends SubSequence
implements CharSeq
{
  public SubCharSeq (AbstractSequence base, int startPos, int endPos)
  {
    super(base, startPos, endPos);
  }

  /** Get length of string, in characters.
   * Synonym for size(), for compatibility with String and StringBuffer. */
  public int length()
  {
    return size();
  }

  public char charAt(int index)
  {
    if (index < 0 || index >=size())
      throw new IndexOutOfBoundsException();
    int start = base.nextIndex(ipos0);
    return ((CharSeq) base).charAt(start + index);
  }

  /** Copy characters into a destination buffer.
   * Same interface as java.lang.String's getChars. */
  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    for (int i = srcBegin;  i < srcEnd;  i++)
      dst[dstBegin++] = charAt(i);
  }

  public void setCharAt(int index, char ch)
  {
    if (index < 0 || index >=size())
      throw new IndexOutOfBoundsException();
    int start = base.nextIndex(ipos0);
    ((CharSeq) base).setCharAt(start + index, ch);
  }

    public void setCharacterAt(int index, int ch) {
        if (index < 0 || index >=size())
            throw new IndexOutOfBoundsException();
        int start = base.nextIndex(ipos0);
        ((CharSeq) base).setCharacterAt(start + index, ch);
    }

  /** Set all the elements to a given character. */
  public void fill(char value)
  {
    int index0 = base.nextIndex(ipos0);
    int index1 = base.nextIndex(ipos0);
    ((CharSeq) base).fill(index0, index1, value);
  }

  public void fill(int fromIndex, int toIndex, char value)
  {
    int index0 = base.nextIndex(ipos0);
    int index1 = base.nextIndex(ipos0);
    if (fromIndex < 0 || toIndex < fromIndex || index0 + toIndex > index1)
      throw new IndexOutOfBoundsException();
    ((CharSeq) base).fill(index0 + fromIndex, index0 + toIndex, value);
  }

  /* #ifdef JAVA5 */
  public void writeTo(int start, int count, Appendable dest)
    throws java.io.IOException
  {
    int index0 = base.nextIndex(ipos0);
    int index1 = base.nextIndex(ipos0);
    if (start < 0 || count < 0 || index0 + start + count > index1)
      throw new IndexOutOfBoundsException();
    ((CharSeq) base).writeTo(index0 + start, count, dest);
  }

  public void writeTo(Appendable dest)
    throws java.io.IOException
  {
    int index0 = base.nextIndex(ipos0);
    ((CharSeq) base).writeTo(index0, size(), dest);
  }
  /* #else */
  // /**
  //  * Write out (part of) this string.
  //  * @param start index of initial character to write
  //  * @param count number of characters to write
  //  * @param dest where to write the characters
  //  */
  // public void writeTo(int start, int count, java.io.Writer dest)
  //   throws java.io.IOException
  // {
  //   int index0 = base.nextIndex(ipos0);
  //   int index1 = base.nextIndex(ipos0);
  //   if (start < 0 || count < 0 || index0 + start + count > index1)
  //     throw new IndexOutOfBoundsException();
  //   ((CharSeq) base).writeTo(index0 + start, count, dest);
  // }

  // public void writeTo(java.io.Writer dest) throws java.io.IOException
  // {
  //   int index0 = base.nextIndex(ipos0);
  //   ((CharSeq) base).writeTo(index0, size(), dest);
  // }
  /* #endif */

  public void consume(int start, int count, Consumer out)
  {
    int index0 = base.nextIndex(ipos0);
    int index1 = base.nextIndex(ipos0);
    if (start < 0 || count < 0 || index0 + start + count > index1)
      throw new IndexOutOfBoundsException();
    ((CharSeq) base).consume(index0 + start, count, out);
  }

  public String toString()
  {
    int sz = size();
    StringBuffer sbuf = new StringBuffer(sz);
    for (int i = 0;  i < sz;  i++)
      sbuf.append(charAt(i));
    return sbuf.toString();
  }

  private SubCharSeq subCharSeq(int start, int end)
  {
    int sz = size();
    if (start < 0 || end < start || end > sz)
      throw new IndexOutOfBoundsException();
    return new SubCharSeq(base,
			  base.createRelativePos(ipos0, start, false),
			  base.createRelativePos(ipos0, end, true));
  }

  /* #ifdef JAVA2 */
  public java.util.List subList(int fromIx, int toIx)
  {
    return (java.util.List) subCharSeq(fromIx, toIx);
  }
  /* #endif */

  /* #ifdef use:java.lang.CharSequence */
  public CharSequence subSequence(int start, int end)
  {
    return subCharSeq(start, end);
  }
  /* #endif */
}
