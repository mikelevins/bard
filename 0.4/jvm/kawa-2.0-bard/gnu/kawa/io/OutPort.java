package gnu.kawa.io;
import java.io.*;
import gnu.mapping.Environment;
import gnu.mapping.ThreadLocation;
import gnu.text.*;
import gnu.lists.*;

/**
 * An extended PrintWriter.
 */

public class OutPort extends PrintConsumer implements Printable
{
  Path path;
  protected Writer base;
  static final int FLUSH_ON_FINALIZE = 1;
  static final int CLOSE_ON_FINALIZE = 2;
  static final int IS_CLOSED = 4;
  int finalizeAction;

  // To keep track of column-numbers, we use a helper class.
  // Otherwise, it is too painful, as there is no documented
  // interface that would allow PrintWriter to be cleanly extended ...
  // The helper class also lets us make transparent use of WriterManager.
  protected PrettyWriter bout;

  /** An index into the WriterManager's internal table.
   * The value zero means it is unregistered. */
  protected WriterManager.WriterRef unregisterRef;

  protected OutPort(Writer base, PrettyWriter out, boolean autoflush)
  {
    super(out, autoflush);
    this.bout = out;
    this.base = base;
    if (closeOnExit())
      unregisterRef = WriterManager.instance.register(this);
  }

  protected OutPort (OutPort out, boolean autoflush)
  {
    this(out, out.bout, autoflush);
  }

  protected OutPort (Writer out, boolean autoflush)
  {
    this(out,
         (out instanceof OutPort ? ((OutPort) out).bout
          : new PrettyWriter(out, true)),
         autoflush);
  }

  public OutPort(Writer base, boolean printPretty, boolean autoflush)
  {
    this(base, new PrettyWriter(base, printPretty), autoflush);
  }

  public OutPort(Writer base, boolean printPretty,
		 boolean autoflush, Path path)
  {
    this(base, new PrettyWriter(base, printPretty), autoflush);
    this.path = path;
  }

  public OutPort (OutputStream out)
  {
    this (out, null);
  }

  public OutPort (OutputStream out, Path path)
  {
    this(new OutputStreamWriter(out), true, path);
  }

  public OutPort (Writer out)
  {
    this(out,
         out instanceof OutPort ? ((OutPort) out).bout
         : new PrettyWriter(out, false),
         false);
  }

  public OutPort (Writer base, Path path)
  {
    this(base, false, false);
    this.path = path;
  }

  public OutPort (Writer base, boolean autoflush, Path path)
  {
    this (base, false, autoflush);
    this.path = path;
  }

  public boolean printReadable;

    static BinaryOutPort outInitial
        = BinaryOutPort.makeStandardPort(System.out, "/dev/stdout");
    static { outInitial.finalizeAction = FLUSH_ON_FINALIZE; }

    private static BinaryOutPort errInitial
        = BinaryOutPort.makeStandardPort(System.err, "/dev/stderr");
    static { errInitial.finalizeAction = FLUSH_ON_FINALIZE; }

    public static BinaryOutPort getSystemOut() { return outInitial; }
    public static BinaryOutPort getSystemErr() { return errInitial; }

  public static final ThreadLocation outLocation
    = new ThreadLocation("out-default");
  static { outLocation.setGlobal(outInitial); }
  public static final ThreadLocation errLocation
    = new ThreadLocation("err-default");
  static { errLocation.setGlobal(errInitial); }
  static public OutPort outDefault ()
  {
    return (OutPort) outLocation.get();
  }

  static public void setOutDefault (OutPort o)
  {
    outLocation.set(o);
  }

  static public OutPort errDefault ()
  {
    return (OutPort) errLocation.get();
  }

  static public void setErrDefault (OutPort e)
  {
    errLocation.set(e);
  }

    public PrettyWriter getPrettyWriter() {
        return bout;
    }

    public static OutPort openFile(Object fname)
        throws java.io.IOException {
        return openFile(fname, Environment.user().get("port-char-encoding"));
    }

    public static OutPort openFile(Object fname, Object conv)
        throws java.io.IOException {
        Path path = Path.valueOf(fname);
        java.io.OutputStream strm = path.openOutputStream();
        strm = new java.io.BufferedOutputStream(strm);
        // Do we need to wrap the OutputStreamWriter in a BufferedWriter?
        // There is buffering in PrettyWriter, but not always.  FIXME.
        OutPort op = conv == Boolean.FALSE
            ? new BinaryOutPort(strm, path)
            : new OutPort(conv == null || conv == Boolean.TRUE
                          ? new OutputStreamWriter(strm)
                          : new OutputStreamWriter(strm, conv.toString()),
                          path);
        op.finalizeAction = CLOSE_ON_FINALIZE;
        return op;
    }

  public void echo (char[] buf, int off, int len)  throws java.io.IOException
  {
    if (base instanceof LogWriter)
      ((LogWriter)base).echo(buf, off, len);
  }

  static Writer logFile;

  public static void closeLogFile ()  throws java.io.IOException
  {
    if (logFile != null)
      {
	logFile.close();
	logFile = null;
      }
    if (outInitial.base instanceof LogWriter)
      ((LogWriter)outInitial.base).setLogFile((Writer) null);
    if (errInitial.base instanceof LogWriter)
      ((LogWriter)errInitial.base).setLogFile((Writer) null);
  }

  public static void setLogFile (String name)  throws java.io.IOException
  {
    if (logFile != null)
      closeLogFile();
    logFile = new PrintWriter(new BufferedWriter(new FileWriter(name)));
    if (outInitial.base instanceof LogWriter)
      ((LogWriter)outInitial.base).setLogFile(logFile);
    if (errInitial.base instanceof LogWriter)
      ((LogWriter)errInitial.base).setLogFile(logFile);
  }

  /*
  public void closeLogFile ()  throws java.io.IOException
  {
    if (base instanceof LogWriter)
      ((LogWriter)base).closeLogFile();
  }

  public void setLogFile (String name)  throws java.io.IOException
  {
    if (base instanceof LogWriter)
      ((LogWriter)base).setLogFile(name);
  }
  */

  protected static final boolean isWordChar(char ch)
  {
    return Character.isJavaIdentifierPart(ch) || ch == '-' || ch == '+';
  }

  //  java.text.FieldPosition fieldPosition;

  /** If non-null, use this to print numbers. */
  java.text.NumberFormat numberFormat;

  public AbstractFormat objectFormat;

  @Override
  public void print(int v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format((long) v));
  }

  @Override
  public void print(long v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format(v));
  }

  @Override
  public void print(double v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format(v));
  }

  @Override
  public void print(float v)
  {
    if (numberFormat == null)
      super.print(v);
    else
      print(numberFormat.format((double) v));
  }

  @Override
  public void print(boolean v)
  {
    if (objectFormat == null)
      super.print(v);
    else
      objectFormat.writeBoolean(v, this);
  }

  @Override
  public void print(String v)
  {
    write(v == null ? "(null)" : v);
  }

  @Override
  public void print(Object v)
  {
    if (objectFormat != null)
      objectFormat.writeObject(v, this);
    else if (v instanceof Consumable)
      ((Consumable) v).consume(this);
    else
      super.print(v == null ? "null" : v);
  }

  public void print (Consumer out)
  {
    out.write("#<output-port");
    if (path != null)
      {
	out.write(' ');
	out.write(path.toString());
      }
    out.write('>');
  }

  @Override
  public void startElement (Object type)
  {
    if (objectFormat != null)
      objectFormat.startElement(type, this);
    else
      {
	print('(');
	print(type);
      }
  }

  @Override
  public void endElement ()
  {
    if (objectFormat != null)
      objectFormat.endElement(this);
    else
      print(')');
  }

  /** Write a attribute for the current element.
   * This is only allowed immediately after a startElement. */
  @Override
  public void startAttribute (Object attrType)
  {
    if (objectFormat != null)
      objectFormat.startAttribute(attrType, this);
    else
      {
        print(' ');
        print(attrType);
        print(": ");
      }
  }

  /** No more attributes in this element. */
  @Override
  public void endAttribute()
  {
    if (objectFormat != null)
      objectFormat.endAttribute(this);
    else
      print(' ');
  }

  /** Note the end of a "word".  See {@link #writeWordStart}. */
  public void writeWordEnd ()
  {
    bout.writeWordEnd();
  }

  /** Maybe write a word-separating space.
   * Specifically, write a space if the previous output
   * was {@link #writeWordEnd}.  Otherwise, do nothing.
   */
  public void writeWordStart ()
  {
    bout.writeWordStart();
  }

  public void freshLine()
  {
    int col = bout.getColumnNumber();
    if (col != 0)
      println();
  }

  /** Get zero-based column number or -1 for unknown. */
  public int getColumnNumber ()
  {
    return bout.getColumnNumber();
  }

  public void setColumnNumber (int column)
  {
    bout.setColumnNumber(column);
  }

    void flushBuffer() {
        bout.forcePrettyOutput();
    }

  public void clearBuffer ()
  {
    bout.clearBuffer();
  }

  /** Flush and close this local Writer, but not underlying Writers. */
  public void closeThis()
  {
    try
      {
        if (! (base instanceof OutPort && ((OutPort) base).bout == bout)) {
          bout.closeThis();
          base = null;
          out = null;
        }
      }
    catch (IOException ex)
      {
        setError();
      }
    WriterManager.instance.unregister(unregisterRef);
    unregisterRef = null;
  }

    public boolean isOpen() { return (finalizeAction & IS_CLOSED) == 0; }

  @Override
  public void close()
  {
    try
      {
        if (base instanceof OutPort && ((OutPort) base).bout == bout) {
          base.close();
          base = null;
        }
        else if (out != null) {
          out.close();
          out = null;
        }
      }
    catch (IOException ex)
      {
        setError();
      }
    WriterManager.instance.unregister(unregisterRef);
    unregisterRef = null;
    finalizeAction = IS_CLOSED;
  }

  /** True if the port should be automatically closed on exit.
   * (If so, it will be registered by WriterManager. */
  protected boolean closeOnExit ()
  {
    return true;
  }

  public void finalize ()
  {
    if ((finalizeAction & FLUSH_ON_FINALIZE) != 0)
      flush();
    if ((finalizeAction & CLOSE_ON_FINALIZE) != 0)
      close();
    else
      closeThis();
  }

  public static void runCleanups ()
  {
    WriterManager.instance.run();
  }

  public void startLogicalBlock (String prefix, boolean perLine,
				 String suffix)
  {
    bout.startLogicalBlock(prefix, perLine, suffix);
  }

    public void startLogicalBlock(String prefix, String suffix,
                                  int indent) {
        synchronized(lock) {
            bout.startLogicalBlock(prefix, false, suffix);
            bout.addIndentation(prefix == null ? indent
                                :  indent - prefix.length(),
                                false);
        }
    }

  public void endLogicalBlock (String suffix)
  {
    bout.endLogicalBlock(suffix);
  }

  public void writeBreak(int kind)
  {
    bout.writeBreak(kind);
  }

    public void writeSpaceLinear() {
        synchronized(lock) {
            write(' ');
            writeBreak(PrettyWriter.NEWLINE_LINEAR);
        }
    }

  /** Write a new-line iff the containing section cannot be printed
   * on one line.  Either all linear-style newlines in a logical
   * block becomes spaces (if it all fits in a line), or none
   * of them do. */
  public void writeBreakLinear()
  {
    writeBreak(PrettyWriter.NEWLINE_LINEAR);
  }

    /** Write a new-line if needed, space otherwise. */
    public void writeSpaceFill() {
        synchronized (lock) {
            write(' ');
            writeBreak(PrettyWriter.NEWLINE_FILL);
        }
    }

  public void writeBreakFill()
  {
    writeBreak(PrettyWriter.NEWLINE_FILL);
  }

  public void setIndentation(int amount, boolean current)
  {
    bout.addIndentation(amount, current);
  }
}
