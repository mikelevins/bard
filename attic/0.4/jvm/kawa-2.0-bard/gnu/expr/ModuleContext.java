// Copyright (c) 2005, 2007, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import java.util.*;
import gnu.kawa.util.AbstractWeakHashTable;

/** Maps modules to module instances.
 * Given a class, species a specific instance object for that class.
 */

public class ModuleContext
{
  static ModuleContext global = new ModuleContext(ModuleManager.instance);
  ModuleManager manager;

  public static int IN_HTTP_SERVER = 1;
  public static int IN_SERVLET = 2;
  int flags;
  public int getFlags () { return flags; }
  public void setFlags(int flags) { this.flags = flags; }
  public void addFlags(int flags) { this.flags |= flags; }

  public ModuleContext (ModuleManager manager)
  {
    this.manager = manager;
  }

  /** For now returns the shared global ModuleContext.
   * Later provide a means for thread-specific overriding. */
  public static ModuleContext getContext ()
  {
    return global;
  }

  public ModuleManager getManager ()
  {
    return manager;
  }

  private ClassToInstanceMap table = new ClassToInstanceMap();

  /** If there is no instance of the argument's class, allocated one. */
  public synchronized Object findInstance (ModuleInfo info)
  {
    Class clas;
    try
      {
        clas = info.getModuleClass();
      }
    catch (java.lang.ClassNotFoundException ex)
      {
        String cname = info.getClassName();
        throw new WrappedException("cannot find module " + cname, ex);
      }
    return findInstance(clas);
  }

  public synchronized Object searchInstance (Class clas)
  {
    return table.get(clas);
  }

  public synchronized Object findInstance (Class clas)
  {
    Object inst = table.get(clas);
    if (inst == null)
      {
        try
          {
            try
              {
                inst = clas.getDeclaredField("$instance").get(null);
              }
            catch (NoSuchFieldException ex)
              {
                // Not a static module - create a new instance.
                inst = clas.newInstance();
              }
          }
        catch (Exception ex)
          {
            throw new WrappedException
              ("exception while initializing module " + clas.getName(), ex);
          }
        setInstance(inst);
      }
    return inst;
  }

  public synchronized void setInstance (Object instance)
  {
    table.put(instance.getClass(), instance);
  }

  public ModuleInfo findFromInstance (Object instance)
  {
    Class instanceClass = instance.getClass();
    synchronized (this)
      {
        ModuleInfo info = manager.findWithClass(instanceClass);
        setInstance(instance);
        return info;
      }
  }

  /** Remove all entries.
   * This can be used to avoids memory leaks.
   */
  public synchronized void clear ()
  {
    table.clear();
  }

  static class ClassToInstanceMap extends AbstractWeakHashTable<Class,Object>
  {
    protected Class getKeyFromValue (Object instance)
    {
      return instance.getClass();
    }

    protected boolean matches (Class oldValue, Class newValue)
    {
      return oldValue == newValue;
    }
  }
}
