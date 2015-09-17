package gnu.text;
import java.text.FieldPosition;
import java.io.Flushable;

/* A Format that does nothing except flush the destination stream. */

public class FlushFormat extends ReportFormat
{
  private static FlushFormat flush;

  public static FlushFormat getInstance()
  {
    if (flush == null)
      flush = new FlushFormat();
    return flush;
  }

    public int format(Object[] args, int start, Appendable dst, FieldPosition fpos)
        throws java.io.IOException {
        if (dst instanceof Flushable)
            ((Flushable) dst).flush();
        return start;
    }
}
