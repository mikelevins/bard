package gnu.jemacs.buffer;
import java.io.*;

/** Inferior process (external command) mode. */

public class InfProcessMode extends ProcessMode
{
  Process proc;

  InputStream out;
  InputStream err;
  OutputStream in;

  public InfProcessMode (Buffer buffer, String command)
  {
    this.buffer = buffer;
    processMark = new Marker(buffer.pointMarker);

    try
      {
	proc = Runtime.getRuntime().exec(command);
      }
    catch (Exception ex)
      {
	throw new gnu.mapping.WrappedException("cannot run "+command, ex);
      }
    in = proc.getOutputStream();
    out = proc.getInputStream();
    err = proc.getErrorStream();
    toInferior = new OutputStreamWriter(in);
    Thread outThread = new InputStreamHandler(out, this);
    outThread.setPriority(Thread.currentThread().getPriority() + 1);
    outThread.start();
    Thread errThread = new InputStreamHandler(err, this);
    errThread.setPriority(Thread.currentThread().getPriority() + 1);
    errThread.start();
  }

  public static void shellMode (Buffer buffer, String command)
  {
    buffer.modes = new InfProcessMode(buffer, command);
  }
}
