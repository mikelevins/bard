// Copyright (c) 2012  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.kawa.io.InPort;
import gnu.text.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.bytecode.Type;

/** Handle special Scheme forms {@code #`}, {@code #'}, and {@code #,}.
 * This is because {@code #,} has two meanings:
 * (1) equivalent to unsyntax when in the context of a quasisyntax form;
 * (2) otherwise a SRFI10 named constructor.
 */

public class ReaderDispatchSyntaxQuote extends ReadTableEntry {

    static Symbol makeSymbol(String name) {
        return Namespace.EmptyNamespace.getSymbol(name);
    }

    static Symbol syntaxSymbol = makeSymbol("syntax");
    static Symbol quasisyntaxSymbol = makeSymbol("quasisyntax");
    static Symbol unsyntaxSymbol = makeSymbol("unsyntax");
    static Symbol unsyntaxSplicingSymbol = makeSymbol("unsyntax-splicing");

    public Object read (Lexer in, int ch, int count)
        throws java.io.IOException, SyntaxException {
        LispReader reader = (LispReader) in;
        switch (ch) {
        case ',':
            if (reader.inQuasiSyntax) {
                return ReaderQuote.read(reader, unsyntaxSymbol,
                                        '@', unsyntaxSplicingSymbol);
            } else {
                return readNamedConstructor(reader);
            }
        case '\'':
            return ReaderQuote.read(reader, syntaxSymbol, '\0', null);
        case '`':
            boolean save = reader.inQuasiSyntax;
            reader.inQuasiSyntax = true;
            try {
                return ReaderQuote.read(reader, quasisyntaxSymbol, '\0', null);
            } finally {
                reader.inQuasiSyntax = save;
            }
        default:
            return null;
        }
    }

    public static Object readNamedConstructor(LispReader reader)
        throws java.io.IOException, SyntaxException {
	InPort port = reader.getPort();
        int length;
        String name;
        Object list;
        if (port.peek() == '('
            && ((length
                 = LList.listLength(list = reader.readObject(), false))
                > 0)
            && ((Pair) list).getCar() instanceof Symbol) {
            name = ((Pair) list).getCar().toString();
            Object proc = ReadTable.getCurrent().getReaderCtor(name);
            if (proc == null)
                reader.error("unknown reader constructor "+name);
            else if (! (proc instanceof Procedure || proc instanceof Type))
                reader.error("reader constructor must be procedure or type name");
            else {
                length--;  // Subtract 1 for the constructor name.
                int parg = proc instanceof Type ? 1 : 0;
                Object[] args = new Object[parg+length];
                Object argList = ((Pair) list).getCdr();
                for (int i = 0;  i < length;  i++) {
                    Pair pair = (Pair) argList;
                    args[parg+i] = pair.getCar();
                    argList = pair.getCdr();
                }
                try {
                    if (parg > 0) {
                        args[0] = proc;
                        return gnu.kawa.reflect.Invoke.make.applyN(args);
                    }
                    return ((Procedure) proc).applyN(args);
                }
                catch (Error ex) {
                    throw ex;
                }
                catch (Throwable ex) {
                    reader.error("caught "+ex+" applying reader constructor "+name);
                }
            }
        }
        else
            reader.error("a non-empty list starting with a symbol must follow #,");
	return Boolean.FALSE;
    }
}
