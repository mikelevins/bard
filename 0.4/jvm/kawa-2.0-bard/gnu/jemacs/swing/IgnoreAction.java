package gnu.jemacs.swing;

/** An Action that does nothing. */

public class IgnoreAction extends javax.swing.text.TextAction
{
  private static IgnoreAction instance = new IgnoreAction("(ignore)");

  public static IgnoreAction getInstance() { return instance; }

  public IgnoreAction(String name)
  {
    super(name);
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
  }
}
