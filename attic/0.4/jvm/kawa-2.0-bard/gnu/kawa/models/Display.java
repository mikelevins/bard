package gnu.kawa.models;
import java.awt.Dimension;
import java.awt.geom.Dimension2D;
import gnu.mapping.ThreadLocation;

/** An abstract "display device".
 * In the AWT/Swing world, this may bundle a GraphicsConfiguration
 * and a Toolkit.
 * In the web servlet world, this may correspond to a browser on
 * on the other end of an http connection. */

public abstract class Display
{
  public static ThreadLocation myDisplay = new ThreadLocation("my-display");

  public static Display getInstance ()
  {
    Object d = myDisplay.get(null);
    if (d instanceof Display)
      return (Display) d;
    String name = d == null ? "swing" : d.toString();
    Class[] noClasses = new Class[0];
    for (;;)
      {
        int comma = name.indexOf(',');
        String rest = null;
        if (comma >= 0)
          {
            rest = name.substring(comma+1);
            name = name.substring(0, comma);
          }

        if (name.equals("swing"))
          name = "gnu.kawa.swingviews.SwingDisplay";
        else if (name.equals("swt"))
          name = "gnu.kawa.swtviews.SwtDisplay";
        else if (name.equals("echo2"))
          name = "gnu.kawa.echo2.Echo2Display";

        try
          {
            Class clas = Class.forName(name);
            java.lang.reflect.Method method
              = clas.getDeclaredMethod("getInstance", noClasses);
            return (Display) method.invoke(null, new Object[0]);
          }
        catch (ClassNotFoundException ex)
          {
            if (rest == null)
              throw new RuntimeException("no display toolkit: "+d);
            name = rest;
          }
        catch (Exception ex)
          {
            throw gnu.mapping.WrappedException.wrapIfNeeded(ex);
          }
      }
  }

  public abstract Window makeWindow ();

  public abstract void addButton (Button model, Object where);

  public abstract void addLabel (Label model, Object where);

  public abstract void addImage (DrawImage model, Object where);

  public void addText (Text model, Object where)
  {
    throw new Error("makeView called on Text");
  }

  public void addSpacer (Spacer model, Object where)
  {
    throw new Error("makeView called on Spacer");
  }

  public abstract void addBox (Box model, Object where);

  public abstract void addView (Object view, Object where);

  public static Dimension asDimension (Dimension2D dim)
  {
    if (dim instanceof Dimension || dim == null)
      return (Dimension) dim;
    else
      return new Dimension((int) (dim.getWidth() + 0.5),
                           (int) (dim.getHeight() + 0.5));
  }

  public Model coerceToModel (Object component)
  {
    if (component instanceof gnu.lists.FString || component instanceof String)
      return new Label(component.toString());
    return (Model) component;
  }
}
