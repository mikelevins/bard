package gnu.kawa.servlet;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.io.InPort;
import gnu.kawa.io.QueueReader;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.io.TtyInPort;
import gnu.lists.PrintConsumer;
import java.io.*;
import gnu.xml.*;

/** The server state for a browser-based "read-eval-print-loop" session. */

public class ReplSession extends Writer
{
  Language language;
  Environment penvironment;
  QueueReader qreader;
  InPort in_p;
  OutBufferWriter err_p;
  OutBufferWriter out_p;
  OutBufferWriter prompt_p;
  Future thread;

  StringBuffer outBuffer = new StringBuffer();
  boolean outAvailable;

  void append1 (char c)
  {
    if (c == '\r' || c == '\n')
      outBuffer.append("<br/>");
    else
      outBuffer.append(c);
  }

  public void write (int c)
  {
    synchronized (this)
      {
        append1((char) c);
      }
  }

  public void write (char[] cbuf, int off, int len)
  {
    synchronized (this)
      {
        for (int i = 0;  i < len; i++)
          append1(cbuf[off+i]);
      }
  }

  public void write (String str, int off, int len)
  {
    synchronized (this)
      {
        for (int i = 0;  i < len; i++)
          append1(str.charAt(off+i));
      }
  }

  public void flush ()
  {
    synchronized (this)
      {
        if (outBuffer.length() > 0)
          outAvailable = true;
        notify();
      }
  }

  public void close ()
  {
    flush();
  }

  String grabOutput ()
  {
    synchronized (this)
      {
        return grabOutputRaw();
      }
  }

  String waitOutput ()
  {
    synchronized (this)
      {
        if (! outAvailable)
          {
            try
              {
                wait(30000);
              }
            catch (Exception ex)
              {
                ex.printStackTrace();
              }
          }
        String out = grabOutputRaw();
        return out;
      }
  }

  String grabOutputRaw ()
  {
    String result = outBuffer.toString();
    outBuffer.setLength(0);
    outAvailable = false;
    return result;
  }

  public ReplSession ()
  {
    this(kawa.standard.Scheme.getInstance());
    //this(gnu.xquery.lang.XQuery.getInstance());
  }

  public ReplSession (Language language)
  {
    if (Language.getDefaultLanguage() == null)
      Language.setDefaults(language);
    penvironment = Environment.getCurrent();
    qreader = new QueueReader();

    out_p = new OutBufferWriter(this, 'O', Path.valueOf("/dev/stdout"));
    err_p = new OutBufferWriter(this, 'E', Path.valueOf("/dev/stderr>"));
    prompt_p = new OutBufferWriter(this, 'P', Path.valueOf("/dev/prompt"));
    in_p = new MyTtyInPort(qreader, Path.valueOf("/dev/stdin"), out_p, this);

    thread = Future.make(new kawa.repl(language),
                         penvironment, in_p, out_p, err_p);
    thread.start();
  }

  void appendInputLine (String line)
  {
    qreader.append(line, 0, line.length());
    qreader.append('\n');
  }

  void appendInput (String line)
  {
    qreader.append(line, 0, line.length());
  }
}

class MyTtyInPort extends TtyInPort
{
  ReplSession session;
  OutBufferWriter prompt_p;
  public MyTtyInPort (Reader in, Path path, OutPort tie,
                      ReplSession session)
  {
    super(in, path, tie);
    this.session = session;
    this.prompt_p = session.prompt_p;
  }

  int pcount;

  public void emitPrompt (String prompt) throws java.io.IOException
  {
    synchronized (session)
      {
        session.out_p.flushToSessionBuffer();
        session.outBuffer.append("<div class=\"interaction\"><span std=\"prompt\">");
        prompt_p.write(prompt);
        prompt_p.flushToSessionBuffer();
        session.outBuffer.append("</span><input std='input' value='' onchange='enterLine(this);'/></div>");
        session.flush();
      }
    tie.clearBuffer();
  }
}

class OutBufferWriter extends XMLPrinter
{
  ReplSession session;
  /** Which port this is:
   * 'O': output
   * 'E': error
   * 'p': prompt
   */
  char kind;
  int nesting = 0;

  public OutBufferWriter (ReplSession session, char kind, Path path)
  {
    super(session, true);
    // setPath(path); // FIXME need to implement super.setPath
    this.session = session;
    this.kind = kind;
  }

  public void startElement (Object type)
  {
    nesting++;
    super.startElement(type);
  }

  public void endElement ()
  {
    nesting--;
    super.endElement();
  }

  final void flushToSessionBuffer ()  throws java.io.IOException
  {
    bout.forcePrettyOutput();
  }

  public void flush ()
  {
    if (nesting > 0)
      return;
    synchronized (session)
      {
        if (kind == 'E')
          session.outBuffer.append("<span std=\"error\">");
        try
          {
            flushToSessionBuffer();
          }
        catch (IOException ex)
          {
            throw new RuntimeException(ex.toString());
          }
        if (kind == 'E')
          session.outBuffer.append("</span>");
        session.flush();
      }
  }
}
