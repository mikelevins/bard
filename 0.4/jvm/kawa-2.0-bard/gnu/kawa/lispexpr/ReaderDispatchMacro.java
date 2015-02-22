// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.math.IntNum;
import gnu.text.Char;
import gnu.text.Lexer;

/** Wrapper for user-supplied reader dispatch macro procedure.
 * This for second-level dispatching, typically after '#'. */

public class ReaderDispatchMacro extends ReaderMisc
{
  Procedure procedure;

  public ReaderDispatchMacro(Procedure procedure)
  {
    super(ReadTable.TERMINATING_MACRO);
    this.procedure = procedure;
  }

  public Procedure getProcedure()
  {
    return procedure;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    // java.io.Reader reader = in;
    java.io.Reader reader = in.getPort();
    try
      {
	return procedure.apply3(reader, Char.make(ch), IntNum.make(count));
      }
    catch (java.io.IOException ex)
      {
	throw ex;
      }
    catch (gnu.text.SyntaxException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	in.fatal("reader macro '"+procedure+"' threw: "+ex);
	return null;  // Never executed.
      }
  }
}
