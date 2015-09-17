// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.mapping.Values;
import gnu.text.Lexer;
import gnu.text.SyntaxException;

public class ReaderMisc extends ReadTableEntry
{
  int kind;

  public ReaderMisc (int kind) { this.kind = kind; }

  public int getKind() { return kind; }

    public Object read(Lexer in, int ch, int count)
	throws java.io.IOException, SyntaxException {
        if (kind == ReadTable.ILLEGAL) {
            String msg = ("invalid character #\\"+((char) ch));  // FIXME
            if (in.isInteractive()) in.fatal(msg);
            else in.error(msg);
        }
        return Values.empty;
    }
}
