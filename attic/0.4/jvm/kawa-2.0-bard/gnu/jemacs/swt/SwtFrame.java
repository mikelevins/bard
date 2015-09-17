//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import java.util.Iterator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import gnu.jemacs.buffer.Buffer;
import gnu.jemacs.buffer.EFrame;
import gnu.lists.FString;
import gnu.lists.FVector;
import gnu.lists.LList;
import gnu.lists.Pair;

/**
 * @author Christian Surlykke
 *         11-07-2004
 */
public class SwtFrame extends EFrame
{

  private Shell shell;
  private SwtWindow swtWindow;
  private Menu menubar;
  
  public SwtFrame ()
  {
    super();
  }

  public SwtFrame (Buffer buffer)
  {
    this(new SwtWindow(buffer, true));
  }

  public SwtFrame (SwtWindow window)
  {
    super(window);
    this.swtWindow = window;
    shell = SwtHelper.newShell(SwtHelper.getDisplay(), new FillLayout());
    swtWindow.getReadyToShow(shell, 0);
  }

  /**
   * @see gnu.jemacs.buffer.EFrame#isLive()
   */
  public boolean isLive()
  {
    // TODO Auto-generated method stub
    return false;
  }

  public void showAboutMessage () 
  {
    Shell shell = new Shell();
    MessageDialog.openInformation(shell, "Information", aboutMessage());
  }

  /**
   * @see gnu.jemacs.buffer.EFrame#ask(java.lang.String)
   */
  public String ask(String prompt)
  {
    Shell shell = new Shell();
    InputDialog inputDialog = new InputDialog(shell, "Jemacs input window", prompt, "", null);
    inputDialog.open();
    String result = inputDialog.getValue();
    inputDialog.close();
    
    return result;
  }

  public Shell getShell()
  {
    return this.shell;
  }

  public void setMenuBar (LList list)
  {
    if (menubar != null)
    {
      SwtHelper.dispose(menubar);
    }

    menubar = SwtHelper.newMenu(shell, SWT.BAR );
    setMenuHelper(menubar, list);
    SwtHelper.setMenuBar(shell, menubar);
  }
  
  /**
   * Heavily inspired from gnu.jemacs.swing.SwingMenu
   */
  private void setMenuHelper(Menu parent, LList list)
  {
    
    for (Iterator iter = list.iterator(); iter.hasNext();)
    {
      Object o = iter.next();
      try
      {
        if (o == null) 
        {
          continue;
        }
        else if (o instanceof Pair)
        {
          CharSequence menuName = (CharSequence) ((Pair) o).getCar();
          MenuItem menuItem = SwtHelper.newMenuItem(parent, SWT.CASCADE, menuName.toString(), null);
          Menu subMenu = SwtHelper.newMenu(menuItem);
          setMenuHelper(subMenu, (LList) ((Pair) o).getCdr());
          SwtHelper.setMenu(menuItem, subMenu);
        }
        else if (o instanceof FVector) 
        {
          CharSequence menuItemName = (CharSequence) ((FVector) o).get(0);
          Object command = ((FVector) o).get(1);
          SwtHelper.newMenuItem(parent, SWT.DROP_DOWN, menuItemName.toString(), new MenuCommandHandler(command));
        }
        else if (o instanceof CharSequence) 
        {
         SwtHelper.newMenuItem(parent, SWT.SEPARATOR, null, null); 
        }
      }
      catch (Exception e) 
      {
        System.err.println("SwtFrame.setMenu - problem with " + o);
      }
    }
  }
  
  class MenuCommandHandler implements SelectionListener
  {
    private Object command;
    
    public MenuCommandHandler(Object command)
    {
      this.command = command;
    }

    public void widgetSelected(SelectionEvent e)
    {
      selectedWindow.handleCommand(command);
    }

    public void widgetDefaultSelected(SelectionEvent e)
    {
      selectedWindow.handleCommand(command);
    }
    
  }
  
}
