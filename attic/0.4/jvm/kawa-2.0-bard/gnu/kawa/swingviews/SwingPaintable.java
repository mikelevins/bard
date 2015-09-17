package gnu.kawa.swingviews;
import java.awt.*;
import java.awt.geom.*;
import gnu.kawa.models.*;
import javax.swing.*;

/** Embeds a Paintable object in a JPanel,. */

public class SwingPaintable extends JPanel
{
  Paintable paintable;
  Dimension dim;

  public SwingPaintable (Paintable paintable)
  {
    this.paintable = paintable;

    Rectangle2D rect = paintable.getBounds2D();
    int h = (int) Math.ceil(rect.getHeight());
    int w = (int) Math.ceil(rect.getWidth());
    dim = new Dimension(w, h);
  }

  public void paint(Graphics g)
  {
    // FIXME may need to transform position
    paintable.paint((Graphics2D) g);
  }

  public java.awt.Dimension getPreferredSize ()
  {
    return dim;
  }
}
