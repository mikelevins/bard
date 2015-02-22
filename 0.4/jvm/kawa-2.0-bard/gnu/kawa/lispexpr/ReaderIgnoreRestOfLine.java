// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.Lexer;
import gnu.text.SyntaxException;
import gnu.lists.Sequence;
import gnu.kawa.io.BinaryInPort;
import gnu.kawa.io.InPort;
import gnu.mapping.Values;

public class ReaderIgnoreRestOfLine extends ReadTableEntry {
    static ReaderIgnoreRestOfLine instance = new ReaderIgnoreRestOfLine();

    public static ReaderIgnoreRestOfLine getInstance() { return instance; }

    public boolean checkEncodingSpec = true;

    public Object read (Lexer in, int ch, int count)
        throws java.io.IOException, SyntaxException {
        InPort port = in.getPort();
        StringBuilder buf = null;
        if (port instanceof BinaryInPort && checkEncodingSpec) {
            int lineno = port.getLineNumber();
            if (lineno == 0 || lineno == 1)
                buf = new StringBuilder();
        }
        do {
            ch = in.read();
            if (ch < 0)
                return Sequence.eofValue;
            if (buf != null)
                buf.append((char) ch);
        } while (ch != '\n' && ch!= '\r');
        if (buf != null) {
            ((LispReader) in).checkEncodingSpec(buf.toString());
        }
        in.unread(ch);
        return Values.empty;
  }
}
