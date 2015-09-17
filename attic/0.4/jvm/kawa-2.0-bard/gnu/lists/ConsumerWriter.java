// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** A Writer that wraps (filters) a Consumer. */

public class ConsumerWriter extends Writer
{
  protected Consumer out;

  public ConsumerWriter(Consumer out)
  {
    this.out = out;
  }

  public void write(char[] buffer, int offset, int length)
  {
    out.write(buffer, offset, length);
  }

  public void flush() { }

  public void close()
  {
    // out.endDocument(); ???
    flush();
  }

  public void finalize()
  {
    close();
  }
}
