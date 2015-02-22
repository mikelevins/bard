//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import gnu.jemacs.buffer.Buffer;
import gnu.jemacs.buffer.Marker;
import gnu.lists.CharSeq;
import gnu.lists.FString;
import gnu.kawa.io.InPort;

import java.io.Reader;
import java.io.Writer;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.swt.custom.StyledTextContent;

/**
 * @author Christian Surlykke
 *         11-07-2004
 */
public class SwtBuffer extends Buffer
{

  private BufferContent bufferContent = null;
    
  public SwtBuffer(String name) {
    this(name, new BufferContent());
  }
  
  public SwtBuffer(String name, BufferContent content) 
  {
    super(name);
    this.bufferContent = content;

    pointMarker = new Marker(this, 0, true);
    markMarker = new Marker();
    redrawModeline();
  }
  
  /** 
   * @see gnu.jemacs.buffer.Buffer#redrawModeline()
   */
  public void redrawModeline()
  {
    modeline = new FString("-----JEmacs: " + getName() + " ---");
  }
  
  private FString modeline = new FString("");
  
  public FString getModelineFormat() {
    return modeline;
  }
  
   /**
   * @see gnu.jemacs.buffer.Buffer#getLength()
   */
  public int getLength()
  {
    return bufferContent.getCharCount();
  }

  
  // ---------------------- The Dot ------------------------------
  private int dot = 0;

  /**
   * @see gnu.jemacs.buffer.Buffer#getDot()
   */
  public int getDot()
  {
    return dot;
  }
  
  /**
   * @see gnu.jemacs.buffer.Buffer#setDot(int)
   */
  public void setDot(int dot)
  {
    this.dot = dot;
  }
  
  /**
   * @see gnu.jemacs.buffer.Buffer#maxDot()
   */
  public int maxDot()
  {
    return length();
  }

  // ---------------------- Insertion --------------------------
  
  public void insert(String string, Object style, int ipos)
  {
    // TODO Auto-generated method stub
  }
 
  /**
   * @see gnu.jemacs.buffer.Buffer#insertChar(int, int, java.lang.Object)
   */
  public void insertChar(int ch, int count, Object style)
  {
    // TODO: Handle styles !
    String str = Marker.repeatChar(ch, count);
    bufferContent.replaceTextRange(getDot(), 0, str);
    setDot(getDot() + 1);
  }

  public void removeRegion (int start, int end)
  {
    bufferContent.replaceTextRange(start, end-start, null);
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#removeAll()
   */
  public void removeAll()
  {
    bufferContent.setText("");
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#insertFile(java.io.Reader)
   */
  public void insertFile(Reader in) throws Exception
  {
    bufferContent.insertFile(in, getDot());
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#save(java.io.Writer)
   */
  public void save(Writer out) throws Exception
  {
    bufferContent.save(out);
  }

  
  
  /**
   * @see gnu.jemacs.buffer.Buffer#removeChar(int)
   */
  public void removeChar(int count)
  {
    int start = Math.min(getDot(), getDot() + count);
    int end = Math.max(getDot(), getDot() + count);
    
    // Confine interval to be within [0; size()]
    start = Math.max(0, Math.min(start, size()));
    end = Math.max(0, Math.min(end, size()));

    if (start != end)
    {
      count = Math.abs(end - start);
      start = Math.min(start, end);
      bufferContent.replaceTextRange(start, count, null);
      setDot(start);
    }
  }
  
  /**
   * @see gnu.jemacs.buffer.Buffer#lineStartOffset(int)
   */
  public int lineStartOffset(int offset)
  {
    return bufferContent.lineStartPos(offset);
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#scan(char, int, int, int, boolean)
   */
  public long scan(char target, int start, int end, int count, boolean allowQuit)
  {
    // TODO Auto-generated method stub
    return 0;
  }

  public CharSeq getStringContent ()
  {
    return bufferContent;
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#openReader(int, int)
   */
  public InPort openReader(int start, int count)
  {
    return new InPort(new BufferContentReader(bufferContent, start, count));
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#savePointMark()
   */
  public long savePointMark()
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#restorePointMark(long)
   */
  public void restorePointMark(long pointMark)
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see gnu.jemacs.buffer.Buffer#invoke(java.lang.Runnable)
   */
  public void invoke(Runnable doRun)
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see gnu.lists.AbstractSequence#size()
   */
  public int size()
  {
    return bufferContent.getCharCount();
  }

  /**
   * @see gnu.lists.AbstractSequence#get(int)
   */
  public Object get(int index)
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see gnu.lists.AbstractSequence#createPos(int, boolean)
   */
  public int createPos(int index, boolean isAfter)
  {
    // TODO Auto-generated method stub
    return 0;
  }

  public StyledTextContent getBufferContent()
  {
    return bufferContent;
  }
  
  /**
   * @see gnu.jemacs.buffer.Buffer#forwardLine(int)
   */
  public int forwardLine(int lines)
  {
    int currentLine = bufferContent.getLineAtOffset(getDot());
    int newLine = currentLine + lines;
    if (newLine < 0)
    {
      setDot(bufferContent.getOffsetAtLine(0));
      return newLine;
    }
    else if (newLine > bufferContent.getLineCount())
    {
      setDot(bufferContent.getOffsetAtLine(bufferContent.getLineCount()));
      return newLine - bufferContent.getLineCount();
    }
    else
    {
      setDot(bufferContent.getOffsetAtLine(newLine));
      return 0;
    }
  }
}
