// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** A Consumer that extends a PrintWriter.  Useful for formatting. */

public class PrintConsumer extends PrintWriter
  implements
  /* #ifdef JAVA5 */
  Appendable,
  /* #endif */
   Consumer
{
  public PrintConsumer(Consumer out, boolean autoFlush)
  {
    super(out instanceof Writer ? (Writer) out : new ConsumerWriter(out),
	  autoFlush);
  }

  public PrintConsumer(OutputStream out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public PrintConsumer(Writer out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public PrintConsumer(Writer out)
  {
    super(out);
  }

  protected void startNumber()
  {
  }

  protected void endNumber()
  {
  }

  /* #ifdef JAVA5 */
  public PrintConsumer append (char c)
  {
    print(c);
    return this;
  }

  public PrintConsumer append (CharSequence csq)
  {
    if (csq == null)
      csq = "null";
    append(csq, 0, csq.length());
    return this;
  }

  public PrintConsumer append (CharSequence csq, int start, int end)
  {
      write(csq == null ? "null" : csq,
            start, end-start);
    return this;
  }
  /* #endif */

    /* #ifdef use:java.lang.CharSequence */
    public void write(CharSequence csq, int start, int length) {
        if (length == 0)
            csq = "";
        if (csq instanceof String)
            write((String) csq, start, length);
        else {
            synchronized (lock) {
                int end = start+length;
                for (int i = start; i < end;  i++)
                    write(csq.charAt(i));
            }
        }
    }
    /* #endif */

    public void writeBoolean(boolean v) {
        print(v);
    }

    public void writeFloat(float v) {
        synchronized (lock) {
            startNumber();
            print(v);
            endNumber();
        }
    }

    public void writeDouble(double v) {
        synchronized (lock) {
            startNumber();
            print(v);
            endNumber();
        }
    }

    public void writeInt(int v) {
        synchronized (lock) {
            startNumber();
            print(v);
            endNumber();
        }
    }

    public void writeLong(long v) {
        synchronized (lock) {
            startNumber();
            print(v);
            endNumber();
        }
    }

  public void startDocument () { }

  public void endDocument() { }

  public void startElement (Object type) { }

  public void endElement () { }

  public void startAttribute (Object attrType) { }

  public void endAttribute() { }

  public void writeObject(Object v)
  {
    print(v);
  }

  public boolean ignoring()
  {
    return false;
  }
}
