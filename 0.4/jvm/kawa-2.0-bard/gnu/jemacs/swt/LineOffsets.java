package gnu.jemacs.swt;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import gnu.lists.GapVector;
import gnu.lists.U32Vector;

/**
 * The purpose of this class is to maintain an ordered set of line offsets for an
 * SwtCharBuffer. 
 * <p>
 * With a LineOffsets instance it's possible to map from the number of a line to the text
 * position where it begins, and back, reasonably fast. 
 * (O(1) for line number to line offset, O(log(#lines)) for line offset to line number)
 * <p>
 * LineOffsets extends GapVector with an U32Vector as base, allowing new line offsets to be inserted
 * quickly during normal text typing.
 * <p>
 * <p>
 * Instances of SwtCharBuffer should hold an instance LineOffsets class and notify it whenever the it's text changes.
 * The notification happens through the methods: 
 * <p>
 * <ul>  
 *   <li>
 *     textRegionMoved, which should be called when the gap (of SwtCharBuffer) changes 
 *     (position or size). 
 *   </li>
 *   <li>
 *     textInserted.
 *   </li>
 *   <li>
 *     textDeleted. 
 *   </li>
 * </ul> 
 * <p>
 * <p>
 * 
 * TODO: decouple this, using a more general event model..
 * 
 * Assume that lineOffset is an instance of LineOffsets, held by swtCharBuffer an instance of 
 * SwtCharBuffer. 
 * <p>
 * Then a value of <code>o</code> at index <code>i</code> in lineOffsets.base
 * means that the line with line number
 * <code>n = (i < lOff.gapStart ? i : i + lOff.gapEnd - lOff.gapStart)</code>  
 * <p>
 * starts at text position 
 * <code>p = (o < swtCB.gapStart ? o : o + swtCB.gapEnd - swtCB.gapStart)</code>
 * <p>
 * @author Christian Surlykke
 *         12-07-2004
 */
public class LineOffsets extends GapVector
{
  
  public final static int minGapSize = 100;
  private static Pattern newLinePattern = Pattern.compile("\n|\r\n|\r"); // We recognize all the usual forms of 
                                                                         // linedelimiters

  
  private U32Vector offsets;
  
  public LineOffsets(int initialSize)
  {
    super(new U32Vector(Math.max(101, initialSize)));
    offsets = (U32Vector) base;
    insertLine(0, 0); // Line 0 allways starts at position 0 -
                      // even if the text is just the empty string
  }
  
  private void setOffset(int index, int Offset)
  {
    offsets.setIntAt(index < gapStart ? index : index + gapEnd - gapStart, Offset);
  }

  private int getOffset(int index)
  {
    return offsets.intAt(index < gapStart ? index : index + gapEnd - gapStart);
  }

  public void insertLine(int index, int offSet)
  {
    gapReserve(index, 1);
    offsets.setIntAt(gapStart++, offSet);
  }

  public int index2offset(int index) 
  {
    return offsets.intAt(index < gapStart ? index : index + gapEnd - gapStart); 
  }
  
  /**
   * We seek the line containing a given text offset using a halfing of intervals algorithm. Therefore
   * the method will use O(log(n)) time, n being the number of lines. 
   */
  public int offset2index(int offset)
  {
    // Adhoc optimization: Very often this class will be asked for the line index belonging to the point
    // where insertion happens, i.e. at the start of the gap. 
    // We try this before the full search so that we may return in O(1) time in this case.
    try 
    {
      if (index2offset(gapStart - 1) <= offset && index2offset(gapStart) > offset)
      {
        return gapStart - 1;
      }
    }
    catch (IndexOutOfBoundsException e)
    { 
    }
    
    // The normal search
    int intervalStart = 0;
    int intervalEnd = size();
  
    // Invariant: offset(intervalStart) <= offset AND offset(intervalEnd) > offset
    while (intervalEnd > intervalStart + 1) 
    {
      int middle = (intervalStart + intervalEnd) / 2;
      if (index2offset(middle) <= offset) 
      {
        intervalStart = middle;
      }
      else 
      {
        intervalEnd = middle;
      }
    }
    
    return intervalStart;
  }

  public void deleteLines(int firstLine, int numberOfLines)
  {
    if (numberOfLines > 0)
    {
      int pos = createPos(firstLine, false);
      removePos(pos, numberOfLines);
      releasePos(pos);
    }
  }

  public void insertLines(int index, int[] offsets)
  {
    if (offsets != null && offsets.length > 0)
    {
      // The offsets should comply with:
      // 0 <= offset[i] < offset[j] for 0 <= i < j
      // 
      
      // TOCONSIDER: 
      // Maybe we should define an exception of our own here?
      
      if (index2offset(index) > offsets[0])
      {
        throw new IllegalArgumentException();
      }
      
      if (index < size() && offsets[offsets.length - 1] > index2offset(index + 1))
      {
        throw new IllegalArgumentException();
      }
      
      for(int i = 0; i < offsets.length -1; i++)
      {
        if (offsets[i] > offsets[i + 1])
        {
          throw new IllegalArgumentException();
        }
      }
      
      gapReserve(index + 1, offsets.length);
      System.arraycopy(offsets, 0, offsets, index + 1, offsets.length);
    }
  }
  
  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("Lines: {" + size() + ", " + gapStart + ", " + gapEnd);
    sbuf.append(" [");
    for (int i = 0; i < size(); i++) 
    { 
      if (i == gapStart)
      {
        sbuf.append("|");
      }
      sbuf.append(offsets.intAt(i < gapStart ? i : i + gapEnd - gapStart));
      
      if (i < size() - 1) 
      {
        sbuf.append(" ");
      }
    }
    sbuf.append("]}");
    return sbuf.toString();
    
  }

  public int countLines(String newText)
  {
    Matcher m = newLinePattern.matcher(newText);
    int i = 0;
    while (m.find())
    {
      i++;
    }
    return i;
  }

  public int linesInRange(int startOffset, int endOffset)
  {
    int indexOfStart = offset2index(startOffset);
    int indexOfEnd = offset2index(endOffset);
    return indexOfEnd - indexOfStart;
  }

  public void textRegionMoved(int regionStart, int regionEnd, int displacement)
  {
    int firstIndexToUpdate = offset2index(regionStart) + 1;
    int lastIndexToUpdate = offset2index(regionEnd);
    for (int index = firstIndexToUpdate; index <= lastIndexToUpdate; index++)
    {
      setOffset(index, getOffset(index) + displacement);
    }
  }

  public void textInserted(int startOffset, CharSequence seq)
  {
    int index = offset2index(startOffset);
    for (Matcher m = newLinePattern.matcher(seq); m.find(); )
    {
      insertLine(++index, startOffset + m.end());
    }
  }

  public void textDeleted(int startOffset, int endOffset)
  {
    int index = offset2index(startOffset);
    shiftGap(index + 1);
    while (gapEnd < offsets.getBufferLength() && offsets.intAt(gapEnd) <= endOffset)
    {
      gapEnd++;
    }
  }

  public boolean isLineDelimiter(char c)
  {
    // TODO Auto-generated method stub
    return c == '\n' || c == '\r';
  }
}
