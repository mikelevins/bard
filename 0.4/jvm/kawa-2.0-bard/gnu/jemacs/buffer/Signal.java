package gnu.jemacs.buffer;

public class Signal extends RuntimeException
{
  String name;
  Object data;

  public Signal(String name, Object data)
  {
    this.name = name;
    this.data = data;
  }

  public static void signal(String name, Object data)
  {
    throw new Signal(name, data);
  }

  public static void signal(String name)
  {
    throw new Signal(name, null);
  }

  public static void error(Object data)
  {
    throw new Signal("error", data);
  }

  public static void message(String msg)
  {
    System.err.println(msg);
  }

  public String toString()
  {
    if (data == null)
      return name;
    else
      return name+": "+data;
  }

  public static void checkQuit()
  {
    if (Thread.interrupted())
      throw new gnu.mapping.WrappedException(new InterruptedException());
  }
}
