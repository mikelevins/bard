package gnu.kawa.swingviews;
import gnu.kawa.models.*;
import gnu.kawa.models.Box;
import javax.swing.*;
import javax.swing.text.*;
import java.awt.Component;
import java.awt.event.*;
import gnu.mapping.Procedure;

public class SwingDisplay extends Display
{
  static final SwingDisplay instance = new SwingDisplay();

  public static Display getInstance() { return instance; }

  public Window makeWindow ()
  {
    SwingFrame window = new SwingFrame(null, null, null);
    window.display = this;
    return window;
  }

  public void addButton (Button model, Object where)
  {
    addView(new SwingButton(model), where);
  }

  public void addLabel (Label model, Object where)
  {
    addView(new SwingLabel(model), where);
  }

  public void addImage (DrawImage model, Object where)
  {
    addView(new JLabel(new ImageIcon(model.getImage())), where);
  }

  public void addText (Text model, Object where)
  {
    // It seems silly that we have to specify an initial value when
    // it's part of the Document, but otherwise Swing doesn't update the view.
    // This means that the text in model.buffer gets deleted and re-inserted,
    // which causes any existing positions to collapse.  FIXME.
    addView(new JTextField(getSwingDocument(model), model.getText(), 50),
            where);
  }

  private static java.util.WeakHashMap documents = null;

  static synchronized javax.swing.text.Document getSwingDocument (Text model)
  {
    if (documents == null)
      documents = new java.util.WeakHashMap();
    Object existing = documents.get(model);
    if (existing != null)
      return (javax.swing.text.Document) existing;
    javax.swing.text.Document doc
      = new javax.swing.text.PlainDocument(new SwingContent(model.buffer));
    documents.put(model, doc);
    return doc;
  }

  public void addBox (Box model, Object where)
  {
    addView(new SwingBox(model, this), where);
  }

  public void addSpacer (Spacer model, Object where)
  {
    addView(new javax.swing.Box.Filler(model.getMinimumSize(),
                                       model.getPreferredSize(),
                                       model.getMaximumSize()),
            where);
  }

  public void addView (Object view, Object where)
  {
    ((java.awt.Container) where).add((Component) view);
  }

  public static ActionListener makeActionListener (Object command)
  {
    if (command instanceof ActionListener)
      return (ActionListener) command;
    return new ProcActionListener((Procedure) command);
  }


  public Model coerceToModel (Object component)
  {
    if (component instanceof Component)
      return new ComponentModel((Component) component);
    if (component instanceof Paintable)
      // Kludge - should create a Viewable.  FIXME.
      return new ComponentModel(new SwingPaintable((Paintable) component));
    return super.coerceToModel(component);
  }
}

class ProcActionListener implements ActionListener
{
  Procedure proc;

  public ProcActionListener (Procedure proc) { this.proc = proc; }

  public void actionPerformed (ActionEvent e)
  {
    try
      {
	proc.apply1(e);
      }
    catch (Throwable ex)
      {
        gnu.mapping.WrappedException.rethrow(ex);
      }
  }
}

class SwingBox
extends javax.swing.Box
implements ModelListener
{
  Box model;

  public SwingBox (Box model, Display display)
  {
    super(model.getAxis());
    model.addListener(this);
    Viewable cellSpacing = model.getCellSpacing();
    int n = model.getComponentCount();
    for (int i = 0;  i < n;  i++)
      {
        if (i > 0 && cellSpacing != null)
          cellSpacing.makeView(display, this);
        model.getComponent(i).makeView(display, this);
      }
  }

  public void modelUpdated (Model model, Object key)
  {
  }
}

class SwingLabel
extends JLabel
implements ModelListener
{
  Label model;

  public SwingLabel (Label model)
  {
    this.model = model;
    String text = model.getText();
    if (text != null)
      super.setText(text);
    model.addListener(this);
  }

  public void modelUpdated (Model model, Object key)
  {
    if (key == "text" && model == this.model)
      super.setText(this.model.getText());
  }

  public void setText(String text)
  {
    if (model == null)
      super.setText(text);
    else
      model.setText(text);
  }
}

/** A Model wrapper around a Swing/AWT Component. */

class ComponentModel extends Model
{
  Component component;

  public ComponentModel (Component component)
  {
    this.component = component;
  }

  public void makeView (Display display, Object where)
  {
    display.addView(component, where);
  }
}
