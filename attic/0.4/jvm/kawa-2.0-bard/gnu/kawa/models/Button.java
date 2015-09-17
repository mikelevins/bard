package gnu.kawa.models;
import java.awt.Color;

/** A model (data) for a clickable button. */

public class Button extends Model
{
  boolean disabled;
  String text;
  Object action;
  // These should be moved to "style" support
  Color foreground;
  Color background;
  Object width;

  public void makeView (Display display, Object where)
  {
    display.addButton(this, where);
  }

  public boolean isDisabled () { return disabled; }
  public void setDisabled (boolean disabled) { this.disabled = disabled; }

  public String getText () { return text; }
  public void setText (String text)
  {
    this.text = text;
    notifyListeners("text");
  }

  public Object getAction () { return action; }
  public void setAction (Object action) { this.action = action; }

  public Color getForeground () { return foreground; }
  public void setForeground (Color fg) 
  {
    foreground = fg;
    notifyListeners("foreground");
  }

  public Color getBackground () { return background; }
  public void setBackground (Color bg)
  {
    background = bg;
    notifyListeners("background");
  }
}
