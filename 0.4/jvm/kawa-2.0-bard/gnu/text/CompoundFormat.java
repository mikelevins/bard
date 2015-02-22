package gnu.text;
import java.text.Format;
import java.text.FieldPosition;
import java.io.Writer;

public class CompoundFormat extends ReportFormat
{
  protected int length;
  protected Format[] formats;

  public CompoundFormat(Format[] formats, int length)
  {
    this.formats = formats;
    this.length = length;
  }

  public CompoundFormat(Format[] formats)
  {
    this.formats = formats;
    this.length = formats.length;
  }

  public int format(Object[] args, int start, Appendable dst, FieldPosition fpos)
    throws java.io.IOException
  {
    for (int i = 0;  i < length;  i++)
      {
	Format fmt = formats[i];
	if (fmt instanceof ReportFormat)
	  {
	    start = ((ReportFormat) fmt).format(args, start, dst, fpos);
	    if (start < 0)
	      return start;
	  }
	else if (start >= args.length)
	  dst.append("#<missing format argument>");
	else
	  {
	    StringBuffer sbuf = new StringBuffer();
	    fmt.format(args[start], sbuf, fpos);
	    dst.append(sbuf.toString());
	    start++;
	  }
      }
    return start;
  }

  public final int format(Object[] args, int start,
			  StringBuffer sbuf, FieldPosition fpos)
  {
    for (int i = 0;  i < length;  i++)
      {
	Format fmt = formats[i];
	if (fmt instanceof ReportFormat)
	  {
	    start = ((ReportFormat) fmt).format(args, start, sbuf, fpos);
	    if (start < 0)
	      return start;	      
	  }
	else
	  {
	    fmt.format(args[start], sbuf, fpos);
	    start++;
	  }
      }
    return start;
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new Error("CompoundFormat.parseObject - not implemented");
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("CompoundFormat[");
    for (int i = 0;  i < length;  i++) 
      { 
	if (i > 0)
	  sbuf.append(", ");
	sbuf.append(formats[i]);
      }
    sbuf.append("]");
    return sbuf.toString();
  }

}
