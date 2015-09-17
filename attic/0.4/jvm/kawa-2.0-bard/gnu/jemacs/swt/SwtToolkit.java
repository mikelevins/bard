//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import org.eclipse.swt.widgets.Menu;

import gnu.jemacs.buffer.Buffer;
import gnu.jemacs.buffer.EFrame;
import gnu.jemacs.buffer.EMenu;
import gnu.jemacs.buffer.EToolkit;
import gnu.lists.LList;

/**
 * @author Christian Surlykke
 */
public class SwtToolkit extends EToolkit
{

  /**
   * @see gnu.jemacs.buffer.EToolkit#newBuffer(java.lang.String)
   */
  public Buffer newBuffer(String name)
  {
    return new SwtBuffer(name);
  }

  /**
   * @see gnu.jemacs.buffer.EToolkit#newFrame(gnu.jemacs.buffer.Buffer)
   */
  public EFrame newFrame(Buffer buffer)
  {
    return new SwtFrame(buffer);
  }

  public EMenu getMenu(LList menubar)
  {
    return new SwtMenu(null);
  }

  /**
   * @see gnu.jemacs.buffer.EToolkit#getFace(java.lang.String, boolean)
   */
  public Object getFace(String name, boolean create)
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see gnu.jemacs.buffer.EToolkit#getIgnoreAction()
   */
  public Object getIgnoreAction()
  {
    // TODO Auto-generated method stub
    return null;
  }

}
