package gnu.lists;
import java.io.*;
import gnu.text.SourceLocator;

/** A <code>Pair</code> with the file name and position it was read from. */

public class PairWithPosition extends ImmutablePair
  implements gnu.text.SourceLocator
{
  String filename;
  /** An encoding of {@code (lineNumber << 12) + columnNumber}.
   * Note if columnNumber is unspecified (0), then position is lineNumber. */
  int position;

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    if (lineno < 0)
      lineno = 0;
    if (colno < 0)
      colno = 0;
    position = (lineno << 12) + colno;
  }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFileName ()
  {
    return filename;
  }

  public String getPublicId ()
  {
    return null;
  }

  public String getSystemId ()
  {
    return filename;
  }

  /** Get the line number of (the start of) this Expression.
    * The "first" line is line 1; unknown is -1. */
  public final int getLineNumber()
  {
    int line = position >> 12;
    return line == 0 ? -1 : line;
  }

  public final int getColumnNumber()
  {
    int column = position & ((1 << 12) - 1);
    return column == 0 ? -1 : column;
  }

  public boolean isStableSourceLocation() { return true; }

  /** Only for serialization. */
  public PairWithPosition ()
  {
  }

  public PairWithPosition (SourceLocator where,
                           Object car, Object cdr)
  {
    super (car, cdr);
    filename = where.getFileName();
    setLine(where.getLineNumber(), where.getColumnNumber());
  }

  public PairWithPosition (Object car, Object cdr)
  {
    super (car, cdr);
  }

  public static PairWithPosition make(Object car, Object cdr,
				      String filename, int line, int column)
  {
    PairWithPosition pair = new PairWithPosition(car, cdr);
    pair.filename = filename;
    pair.setLine(line, column);
    return pair;
  }

  public static PairWithPosition make(Object car, Object cdr,
				      String filename, int position)
  {
    PairWithPosition pair = new PairWithPosition(car, cdr);
    pair.filename = filename;
    pair.position = position;
    return pair;
  }

    /** Should only be used when initializing a PairWithPosition instance. */
    public void init(Object car, Object cdr,
                     String filename, int position) {
        this.car = car;
        this.cdr = cdr;
        this.filename = filename;
        this.position = position;
    }

  /**
   * @serialData Write the car followed by the cdr,
   *   followed by filename (as an Object, so it can be shared),
   *   followed by position (line|(column<<20)).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(car);
    out.writeObject(cdr);
    out.writeObject(filename);
    out.writeInt(position);
  }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        Object car = in.readObject();
        Object cdr = in.readObject();
        String filename = (String) in.readObject();
        int position = in.readInt();
        init(car, cdr, filename, position);
    }
}
