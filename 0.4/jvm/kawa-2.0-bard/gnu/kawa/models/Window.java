package gnu.kawa.models;

/** Represents an abstract (toolkit-independent) window. */

public interface Window
{
  public Display getDisplay ();

  public void setContent (Object content);

  public void setTitle (String title);

  public void setMenuBar (Object menubar);

  public String getTitle ();

  public void open ();
}
