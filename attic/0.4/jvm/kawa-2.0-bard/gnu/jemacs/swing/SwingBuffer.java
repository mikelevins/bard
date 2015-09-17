// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swing;

import gnu.jemacs.buffer.*;
import gnu.mapping.*;
import gnu.kawa.io.InPort;
import gnu.lists.CharSeq;
import java.awt.Color;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import javax.swing.text.*;

/** An Emacs buffer implemented using the Swing toolkits. */

public class SwingBuffer extends Buffer
{
  public DefaultStyledDocument doc;

  public Caret curPosition = null;

  public BufferContent content;
  public StyledDocument modelineDocument;

    public static javax.swing.text.StyleContext styles
        = new javax.swing.text.StyleContext();
    // The style name "default" is special in Swing - it inherits
    // from the current Look & Feel - but we prefer a MonoSpace font.
    static public Style defaultStyle = styles.addStyle("plain",null);
    public Style inputStyle = defaultStyle;
    public static Style redStyle = styles.addStyle("red", null);
    static Style blueStyle = styles.addStyle("blue", null);
    static {
        StyleConstants.setFontFamily(defaultStyle,
                                     "Lucida Sans TypeWriter");
        StyleConstants.setFontSize(defaultStyle, 14);
        StyleConstants.setForeground(redStyle, Color.red);
        StyleConstants.setForeground(blueStyle, Color.blue);
    }

  public SwingBuffer(String name)
  {
    this(name, new BufferContent());
  }

  public SwingBuffer(String name, BufferContent content)
  {
    super(name);
    doc = new DefaultStyledDocument(content, styles);
    this.content = content;

    pointMarker = new Marker(this, 0, true);
    markMarker = new Marker();

    modelineDocument
      = new javax.swing.text.DefaultStyledDocument(new javax.swing.text.StringContent(), styles);
    // Needed for proper bidi (bi-directional text) handling.
    // Does cause extra overhead, so should perhaps not be default.
    // Instead only set it if we insert Hebrew/Arabic text?  FIXME.
    doc.putProperty("i18n", Boolean.TRUE);
    redrawModeline();
  }

  public void removeRegion (int start, int end)
  {
    try
      {
	doc.remove(start, end - start);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
	throw new gnu.mapping.WrappedException(ex);
      }
  }

  public void removeChar (int count)
  {
    remove(getDot(), count);
  }

  void remove (int point, int count)
  {
    try
      {
        if (count < 0)
          {
            count = - count;
	    if (point - count < minDot())
	      Signal.signal("Beginning of buffer");
            point -= count;
          }
	else
	  {
	    if (point + count > maxDot())
	      Signal.signal("End of buffer");
	  }
        doc.remove(point, count);

	// Should not be needed, but seems to be.  Otherwise, getDot()
	// returns its *old* value, which is `count' characters too high.
	// The problem seesm to be that Swing does not properly update
	// a Windows's caret position when the underlying Document has text
	// removed.  Unfortunately, this fix probably won't do the right
	// thing for *other windows* that reference the same buffer.  FIXME.
	// (Strangely, the correct thing seems to happen for insertions.)
	setDot(point);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("bad location: "+ex);
      }
    //doc.remove(index, count);
  }

  public void removePos(int ipos, int count)
  {
    remove(nextIndex(ipos), count);
  }

  public void save(Writer out)
    throws Exception
  {
    int length = getLength();
    int todo = length;
    Segment segment = new Segment();
    int offset = 0;
    while (offset < length)
      {
        int count = length;
        if (count > 4096)
          count = 4096;
        doc.getText(offset, count, segment);
        out.write(segment.array, segment.offset, segment.count);
        offset += count;
      }
  }

  public void insertChar (int ch, int count)
  {
    pointMarker.insertChar(ch, count, inputStyle);
  }

  public void insert (int index, String string, Object style)
  {
    if (style == null)
      style = defaultStyle;
    try
      {
        doc.insertString(index, string, (Style) style);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("bad location: "+ex);
      }
  }

  public void insert (String string, Object style)
  {
    insert(getDot(), string ,style);
  }

  public void insert (String string, Object style, int ipos)
  {
    insert (nextIndex(ipos), string, style);
  }

  public void insertFile(Reader in)
    throws Exception
  {
    char[] buffer = new char[2048];
    int offset = getDot();
    for (;;)
      {
        int count = in.read(buffer, 0, buffer.length);
        if (count <= 0)
          break;
        doc.insertString(offset, new String(buffer, 0, count), null);
        offset += count;
      }
  }

  public void redrawModeline ()
  {
    try
      {
        modelineDocument.remove(0, modelineDocument.getLength());
        
        modelineDocument.insertString(0, "-----", redStyle);
        modelineDocument.insertString(modelineDocument.getLength(),
                                      "JEmacs: " + getName(),
                                      blueStyle);
        modelineDocument.insertString(modelineDocument.getLength(),
                                      " ---",
                                      redStyle);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("internal error in redraw-modeline- "+ex);
      }
  }

  public long scan(char target, int start, int end,
		   int count, boolean allowQuit)
  {
    if (end == 0)
      end = count > 0 ? content.length() - 1 : 0;
    return content.scan(target, start, end, count, allowQuit);
  }

  public int lineStartOffset(int offset)
  {
    return (int) content.scan('\n', offset, minDot(), -1, true);
  }

  public int getDot()
  {
    if (pointMarker.sequence == null)
      return curPosition.getDot();
    else
      return pointMarker.getOffset();
  }

  public void setDot(int i)
  {
    if (i > maxDot())
      throw new Error("set dot to "+i+ " max:"+maxDot());
    if (pointMarker.sequence == null)
      curPosition.setDot(i);
    else
      pointMarker.set(this, i);
  }

  public int maxDot()
  {
    // Subtract 1 for the content's final "\n".
    return content.length() - 1;
  }

  public int getLength()
  {
    return doc.getLength();
  }

  public CharSeq getStringContent ()
  {
    return content.buffer;
  }

  public int createPos(int index, boolean isAfter)
  {
    return content.buffer.createPos(index, isAfter);
  }

  public Object get (int index)
  {
    return content.buffer.get(index);
  }

  public int size ()
  {
    return content.buffer.size();
  }

  public int nextIndex(int ipos)
  {
    return content.buffer.nextIndex(ipos);
  }

  public long savePointMark ()
  {
    int pointPosition = content.buffer.createPos(getDot(), false);
    int markPosition = 0;  // FIXME
    return ((long) markPosition) << 32 | ((long) pointPosition & 0xffffffffl);
  }

  public void restorePointMark (long pointMark)
  {
    int pointPosition = (int) pointMark;
    int markPosition = (int) (pointMark >> 32);
    setDot(content.buffer.nextIndex(pointPosition));
    content.buffer.releasePos(pointPosition);
    // Restore mark - FIXME
    // content.releasePosition(markPosition);
  }

  public InPort openReader (int start, int count)
  {
    return new BufferReader(content.buffer, getPath(), start, count);
  }

  /**
   * @see gnu.jemacs.buffer.Buffer#invoke(java.lang.Runnable)
   */
  public void invoke(Runnable doRun)
  {
    try
    {
      javax.swing.SwingUtilities.invokeAndWait(doRun);
    }
    catch (Exception e)
    {
      throw new WrappedException(e);
    }
  }


}
