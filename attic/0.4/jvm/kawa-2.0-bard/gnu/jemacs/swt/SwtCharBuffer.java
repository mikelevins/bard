//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import gnu.lists.FString;
import gnu.lists.GapVector;

/**
 * TODO Explain that this is all about.
 * @author Christian Surlykke
 *         18-07-2004
 */
public class SwtCharBuffer extends GapVector implements CharSequence
{
  protected FString chars;
  protected LineOffsets lineOffsets;
  
  public SwtCharBuffer(int initialSize)
  { 
    super(new FString(new char[initialSize]));
    chars = (FString) base;
    this.lineOffsets = new LineOffsets(initialSize/50);
  }
    
  /* #ifdef use:java.lang.CharSequence */
  public CharSequence subSequence(int start, int end)
  {
    return new SubSequence(start, end);
  }

  class SubSequence implements CharSequence
  {
    private int start;
    private int end;

    public SubSequence(int start, int end)
    {
      this.start = start;
      this.end = end;
    }
    
    /**
     * @see java.lang.CharSequence#length()
     */
    public int length()
    {
      return end - start;
    }

    /**
     * @see java.lang.CharSequence#charAt(int)
     */
    public char charAt(int index)
    {
      return SwtCharBuffer.this.charAt(index + start);
    }

    /**
     * @see java.lang.CharSequence#subSequence(int, int)
     */
    public CharSequence subSequence(int start, int end)
    {
      return SwtCharBuffer.this.subSequence(this.start + start, this.start + end);
    }
  }
  /* #endif */

  public int pos2offset(int pos)
  {
    return pos < gapStart ? pos : pos + gapEnd - gapStart; 
  }
  
  public int offset2pos(int offset)
  {
    return offset <= gapStart ? offset : offset - gapEnd + gapStart;
  }
  
  /**
   * @see gnu.lists.GapVector#gapReserve(int)
   */
  protected void gapReserve(int where, int needed)
  {
    int oldGapSize = gapEnd - gapStart;
    super.gapReserve(where, needed);
    int newGapSize = gapEnd - gapStart;
    if (newGapSize > oldGapSize)   // It never shrinks
    {
      lineOffsets.textRegionMoved(gapStart + oldGapSize, size(), newGapSize - oldGapSize);
    }
  }
  /**
   * @see gnu.lists.GapVector#shiftGap(int)
   */
  protected void shiftGap(int newGapStart)
  {
    int oldGapStart = gapStart;
    super.shiftGap(newGapStart);
    if (oldGapStart != gapStart)
    {
      int regionStart = oldGapStart < newGapStart ? oldGapStart + gapEnd - gapStart : gapStart;
      int regionEnd = oldGapStart < newGapStart ? gapEnd : oldGapStart;
      int displacement = oldGapStart < newGapStart ? gapStart - gapEnd : gapEnd - gapStart;
      lineOffsets.textRegionMoved(regionStart, regionEnd, displacement);
    
    }
  }
  /**
   * @see java.lang.CharSequence#length()
   */
  public int length()
  {
    return size();
  }
  
  /**
   * @see java.lang.CharSequence#charAt(int)
   */
  public char charAt(int index)
  {
    char c = chars.charAt(index < gapStart ? index : index + gapEnd - gapStart);
    return c;
  }
  /**
   * @param where
   * @param str
   */
  public void insert(int where, String str)
  {
    gapReserve(where, str.length());
    str.getChars(0, str.length(), chars.data, where);
    gapStart += str.length();
    lineOffsets.textInserted(where, str);
  }
  
  /**
   * @param where
   * @param count
   */
  public void delete(int where, int count)
  {
    shiftGap(where + count);
    gapStart -= count;
    lineOffsets.textDeleted(where, where + count);
  }

  public void getChars(int start, int end, char[] dest, int destStart)
  {
    int startOffset = pos2offset(start);
    int endOffset   = pos2offset(end);
    // First we get the part of the string before the gap
    int startOffset1 =  Math.min(startOffset, gapStart);
    int len1 = Math.min(endOffset, gapStart) - startOffset1;
    System.arraycopy(chars.data, startOffset1, dest, destStart, len1);
        
    // Then the part after the gap
    int startOffset2 = Math.max(startOffset, gapEnd);
    int len2 = Math.max(endOffset, gapEnd) - startOffset2;
    System.arraycopy(chars.data, startOffset2, dest, destStart + len1, len2);
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("Text: {" + size() + ", " + gapStart + ", " + gapEnd);
    sbuf.append(" [");
    for (int i = 0; i < size(); i++) 
    { 
      if (i == gapStart)
      {
        sbuf.append("|");
      }
      char c = chars.charAt(i < gapStart ? i : i + gapEnd - gapStart);
      if (c == '\r') c = 'R';
      if (c == '\n') c = 'N';
      sbuf.append(c);
    }
    sbuf.append("]}\n");
    sbuf.append(lineOffsets.toString());
    
    return sbuf.toString();
    

  }

  
  /**
   * 
   */
  protected void show()
  {
    System.out.println(this);
  }

  protected static String printable(String s)
  {
    return ">>" + s.replaceAll("\r", "R").replaceAll("\n", "N") + "<<";
  }

   
}
