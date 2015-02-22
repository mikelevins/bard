package gnu.kawa.models;

/** A "label" may have some text and/or an icon.
 * It probably should be a combination of text and image. */

public class Label extends Model
  implements Viewable, java.io.Serializable
{
  String text;

  public Label ()
  {
  }

  public Label (String text)
  {
    this.text = text;
  }

  public void makeView (Display display, Object where)
  {
    display.addLabel(this, where);
  }

  public String getText()
  {
    return text;
  }

  public void setText (String text)
  {
    this.text = text;
    notifyListeners("text");
  }
}
