package kawa.standard;
import gnu.kawa.io.InPort;
import gnu.lists.FString;
import gnu.mapping.*;

public class read_line
{
  public static Object apply(InPort in, String handling)
    throws java.io.IOException
  {
    int ch = in.read();
    if (ch < 0)
      return gnu.expr.Special.eof;
    int start = in.pos - 1;
    int pos = start;
    int limit = in.limit;
    char[] buffer = in.buffer;
    int delim = -1;  // Length of delimiter.

    // First do a quick scan of what is in in's input buffer.
    while (pos < limit)
      {
        ch = buffer[pos++];
        if (ch == '\r' || ch == '\n')
          {
            pos--;
            if (handling == "trim" || handling == "peek")
              {
                if (handling == "peek")
                  delim = 0;
                if (ch == '\n')
                  delim = 1;
                else if (pos+1 < limit)
                  delim = buffer[pos+1] == '\n' ? 2 : 1;
                else
                  break;
                in.pos = pos + delim;
              }
            else if (handling == "concat" && ch == '\n')
              {
                in.pos = ++pos;
              }
            else
              break;
            return new FString(buffer, start, pos - start);
          }
      }

    // Ok, we haven't found the end-of-line yet, so use the general
    // readLine method in InPort.
    StringBuffer sbuf = new StringBuffer(100);
    if (pos > start)
      sbuf.append(buffer, start, pos - start);
    in.pos = pos;
    char mode = handling == "peek" ? 'P'
      : handling == "concat" || handling == "split" ? 'A'
      : 'I';
    in.readLine(sbuf, mode);
    int length = sbuf.length();
    if (handling == "split")
      {
        if (length == 0)
          delim = 0;
        else
          {
            char last = sbuf.charAt(length - 1);
            if (last == '\r')
              delim = 1;
            else if (last != '\n')
              delim = 0;
            else if (last > 2 && sbuf.charAt(length-2) == '\r')
              delim = 2;
            else
              delim = 1;
            length -= delim;
          }
      }
    FString dataStr = new FString(sbuf, 0, length);
    if (handling == "split")
      {
        FString delimStr = new FString(sbuf, length-delim, delim);
        return Values.values2(dataStr, delimStr);
      }
    else
      return dataStr;
  }
}
