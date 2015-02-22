package kawa;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.text.*;
import gnu.mapping.*;
import gnu.kawa.io.OutPort;
import gnu.kawa.models.Viewable;
import gnu.kawa.models.Paintable;
import gnu.kawa.swingviews.SwingDisplay;

/** A JTextPane for a read-eval-print-loop.  Also creates an
 * out and err PrintWriter so that you can redirect stdout/stderr to
 * these streams, using the System.setOut/setErr methods.
 *
 * @author 	Albert Ting
 * @author      Per Bothner
 */
public class ReplPane extends JTextPane
  implements KeyListener
{
  ReplDocument document;

  /**
   * simple TextArea that always scrolls to the bottom.  Also creates an
   * out and err PrintWriter so that you can redirect stdout/stderr to
   * these streams, using the System.setOut/setErr methods.
   */
  public ReplPane(ReplDocument document)
  {
    super(document);
    this.document = document;
    document.pane = this;
    document.paneCount++;

    addKeyListener(this);
    addFocusListener(document);

    setCaretPosition(document.outputMark);
  }

  @Override
  protected EditorKit createDefaultEditorKit() {
    return new ReplEditorKit(this);
  }

  /** This method is called by the toolkit when the component is removed.
   * Used as a hook to decrement ReplDocument's reference count,
   * and maybe close the document.
   */
  @Override
  public void removeNotify()
  {
    super.removeNotify();
    if (--document.paneCount == 0)
      document.close();
  }

  @Override
  public MutableAttributeSet getInputAttributes()
  {
    return ReplDocument.inputStyle;
  }

  public void keyPressed(KeyEvent e) {
    int code = e.getKeyCode();
    if (code == KeyEvent.VK_ENTER)
      {
	document.enter();
	e.consume();
      }
  }
  public void keyReleased(KeyEvent e) {
  }

  public void keyTyped(KeyEvent e) {
  }

  public OutPort getStdout() {
    return document.out_stream;
  }
  public OutPort getStderr() {
    return document.err_stream;
  }

  public static final String ViewableElementName = "Viewable";
  public static final String PaintableElementName = "Paintable";
  public static final Object ViewableAttribute =
    new String(ViewableElementName);
  public static final Object PaintableAttribute =
    new String(PaintableElementName);
}

class ReplEditorKit extends StyledEditorKit {
  ViewFactory styledFactory;
  ViewFactory factory;
  final ReplPane pane;

  public ReplEditorKit(final ReplPane pane)
  {
    this.pane = pane;
    styledFactory = super.getViewFactory();
    factory = new ViewFactory ()
      {
        public View create(Element elem)
        {
          String kind = elem.getName();
          if (kind == ReplPane.ViewableElementName)
            {
              return (new ComponentView(elem)
                {
                  @Override
                  protected Component createComponent()
                  {
                    AttributeSet attr = getElement().getAttributes();
                    JPanel panel = new JPanel();
                    Viewable v = (Viewable) attr.getAttribute(ReplPane.ViewableAttribute);
                    Component comp;
                    // A kludge: We create a panel, and then since all current
                    // Viewables just create a Component and put it in the
                    // panel, we get rid of the useless JPanel.
                    v.makeView(SwingDisplay.getInstance(), panel);
                    if (panel.getComponentCount() == 1)
                      {
                        comp = panel.getComponent(0);
                        panel.removeAll();
                      }
                    else
                      {
                        panel.setBackground(pane.getBackground());
                        comp = panel;
                      }
                    return comp;
                  }
                });
            }
          else if (kind == ReplPane.PaintableElementName)
            {
              AttributeSet attr = elem.getAttributes();
              return new PaintableView(elem, (Paintable) attr.getAttribute(ReplPane.PaintableAttribute));
            }
          return styledFactory.create(elem);
        }
      };
  }

  @Override
  public ViewFactory getViewFactory ()
  {
    return factory;
  }
}

class PaintableView extends View
{
  Paintable p;
  Rectangle2D bounds;
  public PaintableView (Element elem, Paintable paintable)
  {
    super(elem);
    this.p = paintable;
    this.bounds = paintable.getBounds2D();
  }

  public void paint(Graphics g, Shape a)
  {
    Graphics2D g2 = (Graphics2D) g;
    Rectangle bounds = a.getBounds();
    AffineTransform saveTransform = g2.getTransform();
    Paint savePaint = g2.getPaint();
    try
      {
        g2.translate(bounds.x, bounds.y);
        g2.setPaint(Color.BLACK); // FIXME
        p.paint(g2);
      }
    finally
      {
        g2.setTransform(saveTransform);
        g2.setPaint(savePaint);
      }
  }

  @Override
  public float getAlignment(int axis)
  {
    switch (axis)
      {
      case View.Y_AXIS:
        return 1;
      default:
        return super.getAlignment(axis);
      }
  }

   public float getPreferredSpan(int axis) {
        switch (axis) {
        case View.X_AXIS:
          return (float) bounds.getWidth();
        case View.Y_AXIS:
          return (float) bounds.getHeight();
        default:
            throw new IllegalArgumentException("Invalid axis: " + axis);
        }
    }

    public Shape modelToView(int pos, Shape a, Position.Bias b) throws BadLocationException {
        int p0 = getStartOffset();
        int p1 = getEndOffset();
        if ((pos >= p0) && (pos <= p1)) {
            Rectangle r = a.getBounds();
            if (pos == p1) {
                r.x += r.width;
            }
            r.width = 0;
            return r;
        }
        throw new BadLocationException(pos + " not in range " + p0 + "," + p1, pos);
    }

   public int viewToModel(float x, float y, Shape a, Position.Bias[] bias) {
        Rectangle alloc = (Rectangle) a;
        if (x < alloc.x + (alloc.width / 2)) {
            bias[0] = Position.Bias.Forward;
            return getStartOffset();
        }
        bias[0] = Position.Bias.Backward;
        return getEndOffset();
    }
}
