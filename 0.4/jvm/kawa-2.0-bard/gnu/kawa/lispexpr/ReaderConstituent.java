// Copyright (c) 2013  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.Lexer;
import gnu.text.SyntaxException;

public class ReaderConstituent extends ReaderMisc {

    public ReaderConstituent(int kind) { super(kind); }

    public Object read(Lexer in, int ch, int count)
	throws java.io.IOException, SyntaxException {
        LispReader reader = (LispReader) in;
        int startPos = reader.tokenBufferLength;
        ReadTable rtable = ReadTable.getCurrent();
        Object result = reader.readAndHandleToken(ch, startPos, rtable);
        return result;
    }
}
