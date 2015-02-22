package gnu.kawa.models;
import gnu.lists.*;

/** An editable sequences of characters and embedded objects.
 * For now, only supports plain text.
 * Conceptually similar to javax.swing.text.Document.
 * May display as a one line "text field" or a multi-line "text area"
 * depending on styling preferences; for now only the former is implemented.
 */

public class Text extends Model
  implements Viewable, java.io.Serializable
{
  public final CharBuffer buffer = new CharBuffer(100);

  public Text ()
  {
    this("");
  }

  public Text (String text)
  {
    buffer.gapEnd = 100-1;
    buffer.getArray()[buffer.gapEnd] = '\n';
    setText(text);
  }

  public void makeView (Display display, Object where)
  {
    display.addText(this, where);
  }

  public String getText ()
  {
    int len = buffer.size() - 1;
    int start = buffer.getSegment(0, len);
    return new String(buffer.getArray(), start, len);
  }

  public void setText (String text)
  {
    int size = buffer.size()-1;
    if (size > 0)
      buffer.delete(0, size);
    buffer.insert(0, text, false);
    notifyListeners("text");
  }

  public CharBuffer getBuffer ()
  {
    return buffer;
  }
}
