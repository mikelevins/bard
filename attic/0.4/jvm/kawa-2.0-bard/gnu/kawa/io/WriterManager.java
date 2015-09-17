// Copyright (c) 2001, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.io;
import java.io.*;

/** Manages a collection of Writers, handling automatic closing.
 * This class is useful for making sure that a Writer is closed (and its
 * buffers flushed) when a VM exits.
 * A WriterManager can be usefully passed to the JDK 1.3 method
 * addShutdownHook in Runtime.
 */

public final class WriterManager implements Runnable
{
  private WriterManager() { }
  public static final WriterManager instance = new WriterManager();

  WriterRef first;

  /** Register a Writer.
   * @return an object that can be passed to {@link #unregister}.
   */
  public synchronized WriterRef register (OutPort port)
  {
    WriterRef ref = new WriterRef(port);
    WriterRef first = this.first; // Copy field to local variable.
    if (first != null)
      {
        ref.next = first;
        first.prev = ref;
      }
    this.first = ref;
    return ref;
  }

  /** Unregister a Writer.
   * @param key the object returned by the correspodning {@link #register}.
   */
  public synchronized void unregister (WriterRef key)
  {
    if (key == null)
      return;
    WriterRef ref = (WriterRef) key;
    WriterRef next = ref.next;
    WriterRef prev = ref.prev;
    if (next != null)
      next.prev = prev;
    if (prev != null)
      prev.next = next;
    ref.next = null;
    ref.prev = null;
    if (ref == first)
      first = next;
  }

  public synchronized void run()
  {
    for (WriterRef ref = first;  ref != null; )
      {
        WriterRef next = ref.next;
        Object port = ref.get();
        if (port != null)
          {
            try
              {
                ((OutPort) port).finalize();
              }
            catch (Exception ex)
              {
                // ignore
              }
          }
        ref = next;
      }
    first = null;
  }

  /** Try to register this as a shutdown hook.
   * @return true on success; false if failure
   */
  public boolean registerShutdownHook()
  {
    try
      {
	Runtime.getRuntime().addShutdownHook(new Thread(this));
	return true;
      }
    catch (Exception ex)
      {
	return false;
      }
  }

  public static class WriterRef
  /* #ifdef JAVA2 */
    extends java.lang.ref.WeakReference
    /* #endif */
  {
    WriterRef next;
    WriterRef prev;

    public WriterRef (Writer wr)
    {
      super(wr);
    }

    int id=++counter; static int counter;
    public String toString() { return "WriterRef#"+id+":"+get(); }
  }
}
