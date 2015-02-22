// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.buffer;
import gnu.lists.LList;
import gnu.mapping.WrappedException;
import java.awt.Color;

public abstract class EToolkit
{
  public static final String swingToolkit = "gnu.jemacs.swing.SwingToolkit";
  public static final String swtToolkit = "gnu.jemacs.swt.SwtToolkit";
  public static String defaultToolkit = swingToolkit;
  static EToolkit instance;
  static Class toolkitClass;

  public static EToolkit getInstance ()
  {
    EToolkit inst = instance;
    if (inst != null)
      return inst;
    String name = System.getProperty("gnu.jemacs.toolkit");
    if (name == null)
      name = defaultToolkit;
    else if (name.equals("swing"))
      name = swingToolkit;
    else if (name.equals("swt"))
      name = swtToolkit;
    return getInstance(name);
  }

  public static synchronized EToolkit getInstance (String toolkitClassname)
  {
    if (instance == null)
      {
	try
	  {
	    if (toolkitClass == null)
	      toolkitClass = Class.forName(toolkitClassname);
	    instance = (EToolkit) toolkitClass.newInstance();
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException(ex);
	  }
      }
    return instance;
  }

  public abstract Buffer newBuffer (String name);

  public abstract EFrame newFrame(Buffer buffer);
  
  public abstract EMenu getMenu(LList menubar);

  /** Get a face with the given name. */
  public abstract Object getFace(String name, boolean create);

  public void setUnderline (Object face, boolean underline)
  {
    // Default is to ignore.
  }

  public void setBold (Object face, boolean bold)
  {
    // Default is to ignore.
  }

  public void setForeground (Object face, Color foreground)
  {
    // Default is to ignore.
  }

  public void setBackground (Object face, Color background)
  {
    // Default is to ignore.
  }

  public abstract Object getIgnoreAction();
}
