package gnu.jemacs.buffer;
import kawa.*;
import gnu.mapping.*;
import java.io.*;

public class TelnetMode extends ProcessMode
{
  public TelnetMode(Buffer buffer, String host)
  {
    this(buffer, host, 23);
  }

  public TelnetMode(Buffer buffer, String host, int port)
  {
    this.buffer = buffer;
    processMark = new Marker(buffer.pointMarker);

    Telnet telnet;
    try
      {
	java.net.Socket socket = new java.net.Socket(host, port);
	telnet = new Telnet(socket, false);
	telnet.request(Telnet.DO, Telnet.SUPPRESS_GO_AHEAD);
	telnet.request(Telnet.WILL, Telnet.SUPPRESS_GO_AHEAD);
	telnet.request(Telnet.DO, Telnet.ECHO);
      }
    catch (Exception ex)
      {
	throw new WrappedException("telnet - creating socket failed", ex);
      }
    toInferior = new OutputStreamWriter(telnet.getOutputStream());
    Thread t = new InputStreamHandler(telnet.getInputStream(), this);
    t.setPriority(Thread.currentThread().getPriority() + 1);
    t.start();
  }

  //protected static final int ENTER_CODE = 1;
  //static Procedure enter = new ModuleMethod(this, ENTER_CODE, "enter", 0);

  public static void telnetMode (Buffer buffer, String host, int port)
  {
    buffer.modes = new TelnetMode(buffer, host, port);
  }

  /*
  public Object apply0(ModuleMethod proc)
  {
    switch (proc.selector)
      {
      case ENTER_CODE:
	enter();
	return Values.empty;
      }
    return super.apply0(proc);
  }
  */

}
