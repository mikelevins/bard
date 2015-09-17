//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import java.util.Iterator;

import gnu.jemacs.buffer.EFrame;
import gnu.jemacs.buffer.EMenu;
import gnu.lists.LList;

import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

/**
 * @author Christian Surlykke
 *         26-07-2004
 */
public class SwtMenu extends Menu implements EMenu
{
  public SwtMenu(EFrame frame)
  {
    super(((SwtFrame) frame).getShell());
  }

  public void setMenu(LList menu)
  {
  }

}
