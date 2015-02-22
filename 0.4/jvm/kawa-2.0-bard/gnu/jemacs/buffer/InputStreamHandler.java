package gnu.jemacs.buffer;
import java.io.*;
import gnu.mapping.*;

/** Insert input from an InputStream into a buffer.
 * This is typically output from an inferior process. */

class InputStreamHandler extends Thread
{
  ProcessMode mode;
  InputStream in;
  BufferWriter wr;
  Reader in_r;
  char[] buffer;

  public InputStreamHandler(InputStream in, ProcessMode mode)
  {
    this.in = in;
    this.in_r = new InputStreamReader(in);
    this.wr = new BufferWriter(mode.processMark, true);
    this.mode = mode;
  }

  public void run()
  {
    try
      {
	buffer = new char[512];
	for (;;)
	  {
	    int avail = in_r.read(buffer);
	    if (avail <= 0)
	      break;
	    wr.buffer = buffer;
	    wr.count = avail;
	    mode.invoke(wr);
	    //wr.write(buffer, 0, avail);
	    //wr.flush();
	  }
	in.close();
      }
    catch (Exception ex)
      {
	throw new WrappedException(ex);
      }
  }
}
