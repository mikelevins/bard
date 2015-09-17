package gnu.kawa.io;
import java.io.*;
import gnu.mapping.Procedure;
import gnu.text.*;

/** An interactive input-port.
    Supports prompting, auto-flush of tied output port, transcripts. */

public class TtyInPort extends InPort
{
  protected OutPort tie;

  protected Procedure prompter;

  /** Get the current prompter function. */

  public Procedure getPrompter () { return prompter; }

  /** Set the prompter function.
   * The argument is called when a new line is read.
   * It is passed one argument (this input port), and should return
   * a string.  That string is printed as the prompt string.  */

  public void setPrompter (Procedure prompter)
  {
    this.prompter = prompter;
  }

  public TtyInPort (InputStream in, Path name, OutPort tie)
  {
    super(in, name);
    setConvertCR(true);
    this.tie = tie;
  }

  public TtyInPort (Reader in, Path name, OutPort tie)
  {
    super(in, name);
    setConvertCR(true);
    this.tie = tie;
  }

  protected boolean promptEmitted;

  @Override
  protected int fill (int len) throws java.io.IOException
  {
    int count = in.read(buffer, pos, len);
    if (tie != null && count > 0)
      tie.echo(buffer, pos, count);
    return count;
  }

  public void emitPrompt (String prompt) throws java.io.IOException
  {
    tie.print(prompt);
    tie.flush();
    tie.clearBuffer();
  }

  public void lineStart (boolean revisited) throws java.io.IOException
  {
    if (! revisited)
      {
        if (tie != null)
          tie.freshLine();
        if (prompter != null)
          {
            try
              {
                Object prompt = prompter.apply1(this);
                if (prompt != null)
                  {
                    String string = prompt.toString();
                    if (string != null && string.length() > 0)
                      {
                        emitPrompt(string);
                        promptEmitted = true;
                      }
                  }
              }
            catch (Throwable ex)
              { throw new java.io.IOException("Error when evaluating prompt:"
                                              + ex); }
          }
      }
  }

  public int read () throws IOException
  {
    if (tie != null)
      tie.flush();
    int ch = super.read();
    if (ch < 0)
      {
	if (promptEmitted & tie != null)
	  tie.println();
      }
    promptEmitted = false;
    return ch;
  }

  public int read (char cbuf[], int off, int len) throws IOException
  {
    if (tie != null)
      tie.flush();
    int count = super.read(cbuf, off, len);
    if (count < 0 && promptEmitted & tie != null)
      tie.println();
    promptEmitted = false;
    return count;
  }

}
