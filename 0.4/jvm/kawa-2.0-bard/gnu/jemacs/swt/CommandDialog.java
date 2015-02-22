//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Christian Surlykke
 *         28-07-2004
 */
public class CommandDialog extends Dialog
{

  /**
   * @param parent
   */
  public CommandDialog(Shell parent)
  {
    super(parent);
  }

  /**
   * @param parent
   * @param style
   */
  public CommandDialog(Shell parent, int style)
  {
    super(parent, style);
  }

  public String getCommand() {
    Shell parent = getParent();
    Shell shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
    shell.setText(getText());
    // Your code goes here (widget creation, set result, etc).
    shell.open();
    Display display = parent.getDisplay();
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch()) display.sleep();
    }
    return "hejsa";
  }
}
