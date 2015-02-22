package kawa;  // For now
import java.io.*;

/**
 * An input stream that handles the telnet protocol.
 * It handles the special telnet sequences starting with the
 * "Interpret As Command".(IAC==255) byte.
 */

public class TelnetInputStream extends FilterInputStream
{
  Telnet connection;

  public TelnetInputStream (InputStream in, Telnet conn)
    throws IOException
  {
    super(in);
    buf = new byte[512];
    this.connection = conn;
  }

  protected byte[] buf;

  int pos;
  int count;

  /** The state of control bytes we have seen. */
  int state = 0;

  int subCommandLength = 0;

  static final int SB_IAC = 400;

  public int read () throws IOException
  {
    for (;;)
      {
	if (pos >= count)
          {
	    int avail = in.available();
	    if (avail <= 0)
	      avail = 1;
	    else if (avail > buf.length - subCommandLength)
	      {
		avail = buf.length - subCommandLength;  // FIXME
	      }
	    avail = in.read(buf, subCommandLength, avail);
	    pos = subCommandLength;
	    count = avail;
	    if (avail <= 0)
	      return -1;
	  }
	int ch = buf[pos++] & 0xff;
	if (state == 0)
	  {
	    if (ch != Telnet.IAC)
	      return ch;
	    state = Telnet.IAC;
	    continue;
	  }
	else if (state == Telnet.IAC)
	  {
	    if (ch == Telnet.IAC)
	      {
		state = 0;
		return Telnet.IAC;
	      }
	    else if (ch == Telnet.WILL
		     || ch == Telnet.WONT
		     || ch == Telnet.DO
		     || ch == Telnet.DONT
		     || ch == Telnet.SB)
	      {
		state = ch;
	      }
	    else if (ch == Telnet.IP)
	      {
		System.err.println("Interrupt Process");
		state = 0;
	      }
	    else if (ch == Telnet.EOF)
	      {
		return -1;
	      }
	    else
	      {
		state = 0; // ???
	      }
	  }
	else if (state == Telnet.WILL || state == Telnet.WONT
		 || state == Telnet.DO || state == Telnet.DONT)
	  {
	    connection.handle (state, ch);
	    state = 0;
	  }
	else if (state == Telnet.SB)
	  {
	    if (ch == Telnet.IAC)
	      state = SB_IAC;
	    else
	      buf[subCommandLength++] = (byte) ch;
	  }
	else if (state == SB_IAC)
	  {
	    if (ch == Telnet.IAC)
	      {
		buf[subCommandLength++] = (byte) ch;
		state = Telnet.SB;
	      }
	    else if (ch == Telnet.SE)
	      {
		connection.subCommand(buf, 0, subCommandLength);
		state = 0;
		subCommandLength = 0;
	      }
	    else // Error?
	      {
		state = 0;
		subCommandLength = 0;
	      }
	  }
	else
	  System.err.println("Bad state "+state);
      }
  }

  public int read (byte[] b, int offset, int length) throws IOException
  {
    if (length <= 0)
      return 0;
    int done = 0;
    if (state != 0 || pos >= count)
      {
	int ch = read();
	if (ch < 0)
	  return ch;
	b[offset++] = (byte) ch;
	done++;
      }
    if (state == 0)
      {
	while (pos < count && done < length)
	  {
	    byte ch = buf[pos];
	    if (ch == (byte) Telnet.IAC)
	      break;
	    b[offset++] = ch;
	    done++;
	    pos++;
	  }
      }
    return done;
  }
}
