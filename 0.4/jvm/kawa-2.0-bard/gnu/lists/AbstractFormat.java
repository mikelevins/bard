package gnu.lists;
import gnu.mapping.*;
import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import java.text.FieldPosition;

public abstract class AbstractFormat extends java.text.Format
{
  protected void write(String str, Consumer out)
  {
    out.write(str);
  }

  public void write (int v, Consumer out)
  {
    out.write(v);
  }

  /** Write a long.
   * The default is to call writeLong on the Consumer. */
  public void writeLong(long v, Consumer out)
  {
    out.writeLong(v);
  }

  /** Write an int.
   * The default is to call writeLong, so sub-classes only need to
   * override the latter. */
  public void writeInt(int i, Consumer out)
  {
    writeLong(i, out);
  }

  public void writeBoolean(boolean v, Consumer out)
  {
    out.writeBoolean(v);
  }

  public void startElement (Object type, Consumer out)
  {
    write("(", out);
    write(type.toString(), out);
    write(" ", out);
  }

  public void endElement (Consumer out)
  {
    write(")", out);
  }

  public void startAttribute (Object attrType, Consumer out)
  {
    write(attrType.toString(), out);
    write(": ", out);
  }

  public void endAttribute(Consumer out)
  {
    write(" ", out);  // FIXME
  }

  public abstract void writeObject(Object v, Consumer out);

  public void format (Object value, Consumer out)
  {
    if (out instanceof OutPort)
      {
	OutPort pout = (OutPort) out;
	AbstractFormat saveFormat = pout.objectFormat;
	try
	  {
	    pout.objectFormat = this;
	    out.writeObject(value);
	  }
	finally
	  {
	    pout.objectFormat = saveFormat;
	  }
      }
    else
      out.writeObject(value);
  }

  public final void writeObject (Object obj, PrintConsumer out)
  {
    writeObject(obj, (Consumer) out);
  }

  public final void writeObject (Object obj, java.io.Writer out)
  {
    if (out instanceof Consumer)
      writeObject(obj, (Consumer) out);
    else
      {
	OutPort port = new OutPort(out, false, true);
	writeObject(obj, (Consumer) out);
	port.closeThis();
      }
  }

  public StringBuffer format(Object val, StringBuffer sbuf, FieldPosition fpos)
  {
    CharArrayOutPort out = new CharArrayOutPort();
    writeObject(val, out);
    sbuf.append(out.toCharArray());
    out.close();
    return sbuf;
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new Error(this.getClass().getName()
                    + ".parseObject - not implemented");
  }
}
