package gnu.kawa.io;
import java.io.*;

/** A class that supports an optional log file that output is duplicated to.
  * This is used to implement the Scheme transcript facility. */

public class LogWriter extends FilterWriter
{
  private Writer log;

  public LogWriter (Writer out)
  {
    super (out);
  }

  public final Writer getLogFile () { return log; }

  public void setLogFile (Writer log)
  {
    this.log = log;
  }

  public void setLogFile (String name)  throws java.io.IOException
  {
    //    try
      {
	log = new PrintWriter(new BufferedWriter(new FileWriter(name)));
      }
  //    catch (??)
      {
      }
  }

  public void closeLogFile () throws java.io.IOException
  {
    if (log != null)
      log.close();
    log = null;
  }

  public void write (int c)  throws java.io.IOException
  {
    if (log != null)
      log.write(c);
    super.write(c);
  }

  public void echo (char buf[], int off, int len)  throws java.io.IOException
  {
    if (log != null)
      log.write(buf, off, len);
  }

  public void write (char buf[], int off, int len)  throws java.io.IOException
  {
    if (log != null)
      log.write(buf, off, len);
    super.write(buf, off, len);
  }

  public void write (String str, int off, int len)  throws java.io.IOException
  {
    if (log != null)
      log.write(str, off, len);
    super.write(str, off, len);
  }

  public void flush ()  throws java.io.IOException
  {
    if (log != null)
      log.flush();
    super.flush();
  }

  public void close ()  throws java.io.IOException
  {
    if (log != null)
      log.close();
    super.close();
  }
}
