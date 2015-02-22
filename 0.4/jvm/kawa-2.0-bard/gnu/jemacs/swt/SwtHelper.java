//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.StyledTextContent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

/**
 * A Class to act as a layer between SwtJemacs and SWT. 
 * SWT requires that (almost) all calls to widget methods and
 * constructors happen in the GUI event loop thread. On the other hand
 * it should be possible in JEmacs/Kawa to create new threads and call functions 
 * that may, in turn, call SwtJemacs. Therefore we must have this layer between 
 * SwtJemacs and SWT. Each method in this class will make sure it is executed in 
 * the GUI loop thread. 
 * 
 * @author Christian Surlykke
 *         25-08-2004
 */
public class SwtHelper
{

  /**
   * Calls menubar.dispose()
   * 
   * @param menubar The menu to dispose
   */
  public static void dispose(final Menu menubar)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      getDisplay().syncExec(new Resultable(){ public void run() { dispose(menubar);}});
    }
    else 
    {
      menubar.dispose();
    }
  }

  /**
   * Creates a new menu bar for a shelln
   */
  public static Menu newMenu(final Shell shell, final int bar)
  {
    if (Thread.currentThread() != getDisplay().getThread()) 
    {
      Resultable res = new Resultable() { public void run() { result = newMenu(shell, bar);}};
      getDisplay().syncExec(res);
      return (Menu) res.result;
    }
    else
    {
      return new Menu(shell, bar);
    }
  }

  /**
   * Creates a new Menu inside a MenuItem.
   */
  public static Menu newMenu(final MenuItem menuItem)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newMenu(menuItem);}};
      getDisplay().syncExec(res);
      return (Menu) res.result;
    }
    else 
    {
      return new Menu(menuItem);
    }
  }

  /**
   * Creates a new MenuItem.
   */
  public static MenuItem newMenuItem(final Menu parent, final int style, final String text, final SelectionListener selectionListener)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newMenuItem(parent, style, text, selectionListener);}};
      getDisplay().syncExec(res);
      return (MenuItem) res.result;
    }
    else
    {
      MenuItem menuItem = new MenuItem(parent, style);
      if (text != null)
      {
        menuItem.setText(text);
      }
      if (selectionListener != null)
      {
        menuItem.addSelectionListener(selectionListener);
      }
      return menuItem;
    }
  }

  /**
   * Sets a menu bar for a shell. The menu must have been created 
   * with the shell as parent
   */
  public static void setMenuBar(final Shell shell, final Menu menubar)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      getDisplay().syncExec(new Runnable() {public void run() {setMenuBar(shell, menubar);}});
    }
    else
    {
      shell.setMenuBar(menubar);
    }
  }

  public static void setMenu(final MenuItem menuItem, final Menu subMenu)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      getDisplay().syncExec(new Runnable() {public void run() {setMenu(menuItem, subMenu);}});
    }
    else
    {
      menuItem.setMenu(subMenu);
    }
  }

  public static Shell newShell(final Display display, final FillLayout layout)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newShell(display, layout);}};
      getDisplay().syncExec(res);
      return (Shell) res.result;
    }
    else
    {
      Shell shell = new Shell(display);
      if (layout != null)
      {
        shell.setLayout(layout);
      }
      shell.open();
      return shell;
    }
  }

  /**
   * Creates a StyledText instance with a given content, and an SwtWindow
   * which will be installed as
   * VerifyKeylistener, FocusListener, KeyListener and Mouselistener
   */
  public static StyledText newStyledText(final Composite parent, 
                                         final int style, 
                                         final StyledTextContent styledTextContent, 
                                         final SwtWindow swtWindow, 
                                         final int firstVisibleLine)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newStyledText(parent, style, styledTextContent, swtWindow, firstVisibleLine);}};
      getDisplay().syncExec(res);
      return (StyledText) res.result;
    }
    else {
      StyledText styledText = new StyledText(parent,  style);
      styledText.setContent(styledTextContent);
      styledText.setTopIndex(firstVisibleLine);
      styledText.addVerifyKeyListener(swtWindow);
      styledText.addFocusListener(swtWindow);
      styledText.addKeyListener(swtWindow);
      styledText.addMouseListener(swtWindow);
      return styledText;
    }
  }

  public static int getTopIndex(final StyledText styledText)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = new Integer(getTopIndex (styledText));}};
      getDisplay().syncExec(res);
      return ((Integer) res.result).intValue();
    }
    else
    {
      return styledText.getTopIndex();
    }
  }
 
  public static int getCaretOffset(final StyledText styledText)
  {
    if (Thread.currentThread() != getDisplay().getThread()) 
    {
      Resultable res = new Resultable() {public void run() {result = new Integer(getCaretOffset (styledText));}};
      getDisplay().syncExec(res);
      return ((Integer)res.result).intValue();
    }
    else
    {
      return styledText.getCaretOffset();
    }
  }
  
  public static Composite getParent(final Control control) 
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = getParent (control);}};
      getDisplay().syncExec(res);
      return (Composite) res.result;
    }
    else
    {
      return control.getParent();
    }
  }

  public static Rectangle getArea(final StyledText styledText) 
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = getArea (styledText);}};
      getDisplay().syncExec(res);
      return (Rectangle) res.result;
    }
    else
    {
      return styledText.getClientArea();
    }
  }
  
  public static int getLineHeight(final StyledText styledText)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result =  new Integer(getLineHeight (styledText));}};
      getDisplay().syncExec(res);
      return ((Integer) res.result).intValue();		
    }
    else
    {
      return styledText.getLineHeight();
    }

  }


  
  
  public static void setCaretOffset(final StyledText styledText, final int offset)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Resultable() {public void run() {setCaretOffset (styledText, offset);}}; 
      getDisplay().syncExec(run);
    }
    else
    {
      styledText.setCaretOffset(offset);
    }
  }
  
  
  public static void setContent(final StyledText styledText, final StyledTextContent bufferContent)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Resultable() {public void run() {setContent (styledText, bufferContent);}}; 
      getDisplay().syncExec(run);
    }
    else
    {
      styledText.setContent(bufferContent);
    }

  }

  public static void showSelection(final StyledText styledText)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Resultable() {public void run() {showSelection (styledText);}}; 
      getDisplay().syncExec(run);
    }
    else
    {
      styledText.showSelection();
    }

  }

  public static void redraw(final Control control)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Resultable() {public void run() {redraw (control);}}; 
      getDisplay().syncExec(run);
    }
    else
    {
      control.redraw();
    }

  }

  public static SashForm newSashForm(final Composite parent, final int style)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newSashForm(parent, style);}};
      getDisplay().syncExec(res);
      return (SashForm) res.result;
    }
    else
    {
      return new SashForm(parent, style);
    }
  }
  
  public static void setParent(final Control control, final Composite parent)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Runnable() {public void run() {setParent(control, parent);}};
    }
    else 
    {
      control.setParent(parent);
    }
  }

  public static void injectSashFormAsParent(final Control child, final int style)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Resultable() {public void run() {injectSashFormAsParent(child, style);}}; 
      getDisplay().syncExec(run);
    }
    else
    {
      
      // We can't just do
      //
      //   Sashform sashForm = new SashForm(child.getParent());
      //   child.setParent(sashForm);
      // 
      // - as that would put the sashForm _last_ in it's parent and
      // not at the same posisition the StyledText was occupying.
      
      Shell dummy = new Shell(getDisplay());
      Composite parent = child.getParent();
      Control[] siblings = child.getParent().getChildren();
      for (int i = 0; i < siblings.length; i++)
      {
        if (siblings[i] == child) 
        {
          SashForm sashForm = new SashForm(dummy, style);
          siblings[i].setParent(sashForm);
        }
        else
        {
          siblings[i].setParent(dummy);
        }
      }
      
      siblings = dummy.getChildren();
      for (int i = 0; i < siblings.length; i++)
      {
        siblings[i].setParent(parent);
      }
    }
  }
  
  public static void setWeights(final SashForm form, final int[] weights)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Resultable() {public void run() {setWeights (form, weights);}}; 
      getDisplay().syncExec(run);
    }
    else
    {
      form.setWeights(weights);
    }

  }

  public static void layout(final Composite composite)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Runnable run = new Resultable() { public void run() { layout(composite); }};
      getDisplay().syncExec(run);
    }
    else
    {
      composite.layout();
    }
  }

  
  private static Display display = null;

  
  public static Display getDisplay()
  {
    if (SwtHelper.display == null)
    {
      Runnable guiLoop = new Runnable()
      {
        public void run()
        {
          SwtHelper.display = new Display();
          
          while (!SwtHelper.display.isDisposed ()) 
          {
            try
            {
              if (!SwtHelper.display.readAndDispatch ())
              {
                SwtHelper.display.sleep ();
              }
            }
            catch (Exception t) 
            {
              t.printStackTrace();
            }
          }
          SwtHelper.display.dispose ();
        }
      };
      
      (new Thread(guiLoop)).start();
      
      while (SwtHelper.display == null)
      {
        try
        {
          Thread.sleep(20);
        }
        catch (InterruptedException ie)
        {
        }
      }
    }
  
    return SwtHelper.display;
  }
  
  /**
   * A variant of Runnable to be used in Display.syncExec when the caller 
   * wants to retrive the result. 
   * 
   */
  public static abstract class Resultable implements Runnable
  {
    public Object result;
  }
}
