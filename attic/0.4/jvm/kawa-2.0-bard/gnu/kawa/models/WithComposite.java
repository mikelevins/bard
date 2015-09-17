package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public class WithComposite implements Paintable
{
  Paintable[] paintable;
  Composite[] composite;

  public static WithComposite make(Paintable paintable, Composite composite)
  {
    WithComposite comp = new WithComposite();
    comp.paintable = new Paintable[] { paintable };
    comp.composite = new Composite[] { composite };
    return comp;
  }

  public static WithComposite make(Paintable[] paintable,
				   Composite[] composite)
  {
    WithComposite comp = new WithComposite();
    comp.paintable = paintable;
    comp.composite = composite;
    return comp;
  }

  public static WithComposite make(Object[] arguments)
  {
    int n = 0;
    for (int i = arguments.length;  --i >= 0; )
      {
	if (arguments[i] instanceof Paintable)
	  n++;
      }
    Paintable[] paintable = new Paintable[n];
    Composite[] composite = new Composite[n];
    Composite comp = null;
    int j = 0;
    for (int i = 0;  i < arguments.length;  i++)
      {
	Object arg = arguments[i];
	if (arg instanceof Paintable)
	  {
	    paintable[j] = (Paintable) arguments[i];
	    composite[j] = comp;
	    j++;
	  }
	else
	  {
	    comp = (Composite) arg;
	  }
      }
    return make(paintable, composite);
      
  }

  public void paint (Graphics2D graphics)
  {
    Composite saved = graphics.getComposite();
    Composite prev = saved;
    try
      {
	int n = paintable.length;
	for (int i = 0;  i < n;  i++)
	  {
	    Composite cur = composite[i];
	    if (cur != null && cur != prev)
	      {
		graphics.setComposite(cur);
		prev = cur;
	      }
	    paintable[i].paint(graphics);
	  }
      }
    finally
      {
	if (prev != saved)
	  graphics.setComposite(saved);
      }
  }

  public Rectangle2D getBounds2D()
  {
    int n = paintable.length;
    if (n == 0)
      return null; // ???
    Rectangle2D bounds = paintable[0].getBounds2D();
    for (int i = 1;  i < n;  i++)
      bounds = bounds.createUnion(paintable[i].getBounds2D());
    return bounds;
  }

  public Paintable transform (AffineTransform tr)
  {
    int n = paintable.length;
    Paintable[] transformed =  new Paintable[n];
    for (int i = 0;  i < n;  i++)
      transformed[i] = paintable[i].transform(tr);
    return WithComposite.make(transformed, composite);
  }
}
