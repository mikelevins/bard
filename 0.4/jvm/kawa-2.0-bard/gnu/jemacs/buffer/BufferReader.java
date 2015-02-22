package gnu.jemacs.buffer;
import gnu.mapping.*;
import java.io.*;
import gnu.kawa.io.InPort;
import gnu.kawa.io.NullReader;
import gnu.kawa.io.Path;
import gnu.lists.CharBuffer;

public class BufferReader extends InPort
{
  CharBuffer content;
  int rangeStart;
  int rangeLength;

  public BufferReader(CharBuffer content, Path path, int start, int count)
  {
    super(NullReader.nullReader, path);
    this.content = content;
    buffer = content.getArray();
    rangeStart = start;
    rangeLength = count;
    if (start < content.gapStart)
      {
	pos = start;
	limit = start + count;
	if (limit > content.gapStart)
	  limit = content.gapStart;
      }
    else
      {
	int gapSize = content.gapEnd - content.gapStart;
	pos = start + gapSize;
	int length = content.getArray().length;
	limit = pos + count > length ? length : pos + count;
      }
  }

  public int read ()
  {
    if (pos < limit)
      return buffer[pos++];
    if (limit == content.gapStart)
      {
	int gapSize = content.gapEnd - content.gapStart;
	pos = content.gapEnd;
	int count = rangeLength - (content.gapStart - rangeStart);
	int length = content.getArray().length;
	limit = pos + count > length ? length : pos + count;
	if (pos < limit)
	  return buffer[pos++];
      }
    return -1;
  }

  // int highestPos - is not used
  // public synchronized void mark (int readAheadLimit) - seems OK.
  // public boolean ready () -- seems OK

  public void reset ()  throws IOException
  {
    if (readAheadLimit < 0)
      throw new IOException ("mark invalid");
    if (pos >= content.gapEnd && markPos <= content.gapStart)
      limit = content.gapEnd;
    pos = markPos;
    readAheadLimit = -1;
  }

  public int getLineNumber ()
  {
    throw new Error("BufferReader.getLineNumber - not implemented");
  }

  public int getColumnNumber ()
  {
    throw new Error("BufferReader.getColumnNumber - not implemented");
  }

}
