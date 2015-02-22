package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public class WithTransform implements Paintable
{
  Paintable paintable;
  AffineTransform transform;

  public WithTransform(Paintable paintable, AffineTransform transform)
  {
    this.paintable = paintable;
    this.transform = transform;
  }

  public void paint (Graphics2D graphics)
  {
    AffineTransform saved = graphics.getTransform();
    try
      {
	graphics.transform(transform);
	paintable.paint(graphics);
      }
    finally
      {
	graphics.setTransform(saved);
      }
  }

  public Rectangle2D getBounds2D()
  {
    return transform.createTransformedShape(paintable.getBounds2D())
      .getBounds2D();
  }

  public Paintable transform (AffineTransform tr)
  {
    AffineTransform combined = new AffineTransform(transform);
    combined.concatenate(tr);
    return new WithTransform(paintable, combined);
  }
}
