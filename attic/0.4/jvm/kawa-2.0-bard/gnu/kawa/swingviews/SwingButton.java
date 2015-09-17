package gnu.kawa.swingviews;
import gnu.kawa.models.*;
import javax.swing.*;
import java.awt.Color;

public class SwingButton
extends JButton
implements ModelListener
{
  Button model;

  public SwingButton (Button model)
  {
    super(model.getText());
    setModel(new SwModel(model));
    this.model = model;
    Object action = model.getAction();
    if (action != null)
      addActionListener(SwingDisplay.makeActionListener(action));
    model.addListener(this);
    Color fg = model.getForeground();
    if (fg != null)
      super.setBackground(fg);
    Color bg = model.getBackground();
    if (bg != null)
      super.setBackground(bg);
  }

  public void setText(String text)
  {
    if (model == null)
      super.setText(text);
    else
      model.setText(text);
  }

  public void setForeground (Color fg)
  {
    if (model == null)
      super.setForeground(fg);
    else
      model.setForeground(fg);
  }

  public void setBackground (Color bg)
  {
    if (model == null)
      super.setBackground(bg);
    else
      model.setBackground(bg);
  }

  public void modelUpdated (Model model, Object key)
  {
    if (key == "text" && model == this.model)
      super.setText(this.model.getText());
    else if (key == "foreground" && model == this.model)
      super.setForeground(this.model.getForeground());
    else if (key == "background" && model == this.model)
      super.setBackground(this.model.getBackground());
  }
}

class SwModel extends DefaultButtonModel
{
  Button model;

  public SwModel (Button model)
  {
    this.model = model;
    setActionCommand(model.getText());
  }
}
