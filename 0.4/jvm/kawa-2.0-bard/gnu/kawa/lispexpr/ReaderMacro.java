// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.text.*;

/** Wrapper for user-supplied reader macro procedure. */

public class ReaderMacro extends ReaderMisc
{
  Procedure procedure;

  public ReaderMacro(Procedure procedure, boolean nonTerminating)
  {
    super(nonTerminating ? ReadTable.NON_TERMINATING_MACRO
	  : ReadTable.TERMINATING_MACRO);
    this.procedure = procedure;
  }

  public ReaderMacro(Procedure procedure)
  {
    super(ReadTable.TERMINATING_MACRO);
    this.procedure = procedure;
  }

  public boolean isNonTerminating()
  {
    return kind == ReadTable.NON_TERMINATING_MACRO;
  }

  public Procedure getProcedure()
  {
    return procedure;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    // java.io.Reader reader = in;
    java.io.Reader reader = in.getPort();
    try
      {
	return procedure.apply2(reader, Char.make(ch));
      }
    catch (java.io.IOException ex)
      {
	throw ex;
      }
    catch (gnu.text.SyntaxException ex)
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
