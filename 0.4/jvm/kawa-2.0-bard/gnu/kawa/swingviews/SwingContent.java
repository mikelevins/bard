package gnu.kawa.swingviews;
import javax.swing.text.*;
import javax.swing.undo.*;
import gnu.lists.*;

/** A wrapper around a CharBuffer that implements Swing's Content.
 * This allows us to use a CharBuffer for a Document's Content. */

public class SwingContent
  implements javax.swing.text.AbstractDocument.Content
{
  public final CharBuffer buffer;

  public SwingContent (CharBuffer buffer)
  {
    this.buffer = buffer;
  }

  public SwingContent (int initialSize)
  {
    CharBuffer b = new CharBuffer(initialSize);
    // Swing assumes that a Content object is initialized to contain
    // a single '\n'.  This of course is not clearly documented ...
    b.gapEnd = initialSize-1;
    b.getArray()[b.gapEnd] = '\n';
    this.buffer = b;
  }

  public SwingContent ()
  {
    this(100);
  }

  public int length () { return buffer.length(); }

  public void getChars(int where, int len, Segment txt)
    throws BadLocationException
  {
    CharBuffer b = buffer;
    int start = b.getSegment(where, len);
    if (start < 0)
      throw new BadLocationException("invalid offset", where);
    txt.offset = start;
    txt.array = b.getArray();
    txt.count = len;
  }

  public String getString(int where, int len)
    throws BadLocationException
  {
    CharBuffer b = buffer;
    int start = b.getSegment(where, len);
    if (start < 0)
      throw new BadLocationException("invalid offset", where);
    return new String(b.getArray(), start, len);
  }

  public UndoableEdit remove(int where, int nitems)
    throws BadLocationException
  {
    CharBuffer b = buffer;
    if (nitems < 0 || where < 0 || where + nitems > b.length())
      throw new BadLocationException("invalid remove", where);

    b.delete(where, nitems);

    GapUndoableEdit undo = new GapUndoableEdit(where);
    undo.content = this;
    undo.data = new String(b.getArray(), b.gapEnd - nitems, nitems);
    undo.nitems = nitems;
    undo.isInsertion = false;
    return undo;
  }

  public UndoableEdit
  insertString(int where, String str, boolean beforeMarkers)
    throws BadLocationException
  {
    CharBuffer b = buffer;
    if (where < 0 || where > b.length())
      throw new BadLocationException("bad insert", where);
    b.insert(where, str, beforeMarkers);

    GapUndoableEdit undo = new GapUndoableEdit(where);
    undo.content = this;
    undo.data = str;
    undo.nitems = str.length();
    undo.isInsertion = true;
    return undo;
  }

  public UndoableEdit insertString(int where, String str)
    throws BadLocationException
  {
    return insertString(where, str, false);
  }

  public javax.swing.text.Position createPosition(int offset)
    throws BadLocationException
  {
    CharBuffer b = buffer;
    if (offset < 0 || offset > b.length())
      throw new BadLocationException("bad offset to createPosition", offset);
    return new GapPosition(b, offset);
  }

}

class GapPosition extends SeqPosition
    implements javax.swing.text.Position
{
  public GapPosition(CharBuffer content, int offset)
  {
    // Swing Position objects have the 'isAfter' property,
    // except for the case when the offset is 0.
    super(content, offset, offset!=0);
  }

  public int getOffset() { return nextIndex(); }
}

class GapUndoableEdit extends AbstractUndoableEdit
{
  // False if this is a remove (delete);  true if an insertion.
  boolean isInsertion;

  SwingContent content;

  String data;

  int startOffset;
  int nitems;

  GapUndoableEdit(int offset)
  {
    startOffset = offset;
  }

  private void doit(boolean isInsertion)
    throws BadLocationException
  {
    //int startOffset = content.positions[content.indexes[startIndex]];
    if (isInsertion)
      {
        // FIXME returns useless Undo
        content.insertString(startOffset, data);
      }
    else
      {
        // FIXME returns useless Undo
        content.remove(startOffset, nitems);
      }
  }

  public void undo () throws CannotUndoException
  {
    super.undo();
    try
      {
        doit (! isInsertion);
      }
    catch (BadLocationException ex)
      {
        throw new CannotUndoException();
      }
  }

  public void redo () throws CannotUndoException
  {
    super.redo();
    try
      {
        doit (isInsertion);
      }
    catch (BadLocationException ex)
      {
        throw new CannotRedoException();
      }
  }
}
