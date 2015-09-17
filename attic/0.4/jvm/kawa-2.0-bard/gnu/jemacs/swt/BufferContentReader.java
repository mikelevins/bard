//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import java.io.IOException;
import java.io.Reader;

/**
 * @author Christian Surlykke
 *         31-07-2004
 */
public class BufferContentReader extends Reader
{

  private boolean closed = false;
  private BufferContent bufferContent;
  private int start;
  private int count;
  
  /**
   * 
   */
  public BufferContentReader(BufferContent bufferContent, int start, int count)
  {
    super();
    this.bufferContent = bufferContent;
    this.start = start;
    this.count = Math.min(bufferContent.size() - start, count);
  }

  /**
   * @see java.io.Reader#close()
   */
  public void close() throws IOException
  {
    closed = true;
  }

  /**
   * @see java.io.Reader#read(char[], int, int)
   */
  public int read(char[] cbuf, int off, int len) throws IOException
  {
    if (closed)
    {
      throw new IOException();
    }
    
    if (count <= 0)
    {
      return -1;
    }
    else
    {
      int chars = Math.min(this.count, len);
      bufferContent.getChars(start, start + chars, cbuf, off);
      start += chars;
      count -= chars;
      return chars;
    }
  }
}
