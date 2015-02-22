package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;
import gnu.kawa.io.Path;
import gnu.mapping.WrappedException;
import java.net.URL;

public class DrawImage extends Model
  implements Paintable, java.io.Serializable
{
  BufferedImage image;
  Path src;
  String description;

  public DrawImage ()
  {
  }

  public void makeView (Display display, Object where)
  {
    display.addImage(this, where);
  }

  void loadImage ()
  {
    if (image == null)
      {
        try
          {
            image = javax.imageio.ImageIO.read(src.openInputStream());
          }
        catch (Exception ex)
          {
            throw WrappedException.wrapIfNeeded(ex);
          }
      }
  }

  public DrawImage (BufferedImage image)
  {
    this.image = image;
  }

  public void paint (Graphics2D graphics)
  {
    loadImage();
    graphics.drawImage(image, null, null);
  }

  public Rectangle2D getBounds2D()
  {
    loadImage();
    int w = image.getWidth();
    int h = image.getHeight();
    return new Rectangle2D.Float(0, 0, w, h);
  }

  public Paintable transform (AffineTransform tr)
  {
    return new WithTransform(this, tr);
  }

  public Image getImage ()
  {
    loadImage();
    return image;
  }

  public Path getSrc () { return src; }

  public void setSrc (Path src)
  {
    this.src = src;
  }
}
