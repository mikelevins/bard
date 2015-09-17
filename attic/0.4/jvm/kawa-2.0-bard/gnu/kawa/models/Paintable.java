package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public interface Paintable // extends Viewable
{
  public void paint(Graphics2D graphics);
  public Rectangle2D getBounds2D();
  public Paintable transform (AffineTransform tr);
}
