package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public class WithPaint implements Paintable
{
  Paintable paintable;
  Paint paint;

  public WithPaint(Paintable paintable, Paint paint)
  {
    this.paintable = paintable;
    this.paint = paint;
  }

  public void paint (Graphics2D graphics)
  {
    Paint saved = graphics.getPaint();
    try
      {
	graphics.setPaint(paint);
	paintable.paint(graphics);
      }
    finally
      {
	graphics.setPaint(saved);
      }
  }

  public Rectangle2D getBounds2D()
  {
    return paintable.getBounds2D();
  }

  public Paintable transform (AffineTransform tr)
  {
    return new WithPaint(paintable.transform(tr), paint);
  }
}
