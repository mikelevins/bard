package kawa;  // For now
import java.io.*;

/**   
 * An input stream tha handles the telnet protocol.
 * Basically, the byte value IAC is doubled.
 * In addition, various utility methods are provided.
 */

public class TelnetOutputStream extends FilterOutputStream
{
  public TelnetOutputStream (OutputStream out)
  {
    super(out);
  }

  public void write (int value) throws IOException
  {
    if (value == Telnet.IAC)
      out.write(value);
    out.write(value);
  }

  public void write (byte[] b) throws IOException
  {
    write(b, 0, b.length);
  }

  public void write (byte[] b, int off, int len) throws IOException
  {
    int i;
    int limit = off + len;
    for (i = off;  i < limit;  i++)
    {
      if (b[i] == (byte) Telnet.IAC)
	{
	  // Write from b[off] upto and including b[i].
	  out.write(b, off, i+1-off);
	  // Next time, start writing at b[i].
	  // This causes b[i] to be written twice, as needed by the protocol.
	  off = i;
	}
    }
    // Write whatever is left.
    out.write(b, off, limit-off);
  }

  public void writeCommand (int code) throws IOException
  {
    out.write(Telnet.IAC);
    out.write(code);
  }

  public final void writeCommand (int code, int option) throws IOException
  {
    out.write(Telnet.IAC);
    out.write(code);
    out.write(option);
  }

  public final void writeDo (int option) throws IOException
  {
    writeCommand(Telnet.DO, option);
  }

  public final void writeDont (int option) throws IOException
  {
    writeCommand(Telnet.DONT, option);
  }

  public final void writeWill (int option) throws IOException
  {
    writeCommand(Telnet.WILL, option);
  }

  public final void writeWont (int option) throws IOException
  {
    writeCommand(Telnet.WONT, option);
  }

  public final void writeSubCommand (int option, byte[] command)
     throws IOException
  {
    writeCommand(Telnet.SB, option);
    write(command);
    writeCommand(Telnet.SE);
  }
}
