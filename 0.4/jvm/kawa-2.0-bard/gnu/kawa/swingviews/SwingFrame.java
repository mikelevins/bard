package gnu.kawa.swingviews;
import gnu.kawa.models.*;
import java.awt.Component;
import java.awt.Container;
import javax.swing.*;
import gnu.lists.*;

public class SwingFrame extends JFrame
implements gnu.kawa.models.Window
{
  SwingDisplay display;

  public Display getDisplay () { return display; }

  public SwingFrame (String title,
		     javax.swing.JMenuBar menubar,
		     Object contents)
  {
    JFrame fr = this;
    if (title != null)
      fr.setTitle(title);
    if (menubar != null)
      fr.setJMenuBar(menubar);
    Container pane = getContentPane();
    pane.setLayout(new BoxLayout(pane, BoxLayout.X_AXIS));
    addComponent(contents);
  }

  public void setContent (Object content)
  {
    setContentPane(new JPanel());
    addComponent(content);
    pack();
  }

  public void setMenuBar (Object menubar)
  {
    setJMenuBar((javax.swing.JMenuBar) menubar);
  }

  public void addComponent (Object contents)
  {
    if (contents instanceof gnu.lists.FString || contents instanceof String)
      getContentPane().add(new JLabel(contents.toString()));
    else if (contents instanceof AbstractSequence)
      {
	AbstractSequence seq = (AbstractSequence) contents;
	for (int iter = seq.startPos();  (iter = seq.nextPos(iter)) != 0; )
	  addComponent(seq.getPosPrevious(iter));
      }
    else if (contents instanceof Viewable)
      ((Viewable) contents).makeView(getDisplay(), getContentPane());
    else if (contents instanceof Paintable)
      getContentPane().add(new SwingPaintable((Paintable) contents));
    else if (contents != null)
      getContentPane().add((Component) contents);
  }

  public void open ()
  {
    pack();
    setVisible(true);
  }
}
