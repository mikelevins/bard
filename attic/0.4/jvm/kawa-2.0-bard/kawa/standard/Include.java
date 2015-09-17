package kawa.standard;

import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.io.*;
import gnu.kawa.lispexpr.LispReader;
import gnu.text.*;
import kawa.lang.*;
import java.io.*;
import java.nio.charset.*;
import java.util.*;

/** Syntax class for source-file inclusion. */

public class Include extends Syntax {
    boolean ignoreCase;
    boolean relative;

    public static final Include include =
        new Include("include", false, false);
    public static final Include includeRelative =
        new Include("include-relative", true, false);
    public static final Include includeCi =
        new Include("include-ci", true, true);

    public Include(String name, boolean relative, boolean ignoreCase) {
        super(name);
        this.relative = relative;
        this.ignoreCase = ignoreCase;
    }

    @Override
    public void scanForm(Pair st, ScopeExp defs, Translator tr) {
        process(st.getCdr(), tr, defs, ignoreCase);
    }

    @Override
    public Expression rewrite(Object obj, Translator tr) {
        return tr.rewrite_body(process(obj, tr, null, ignoreCase));
    }

    public LList process(Object rest, Translator tr, ScopeExp defs,
                         boolean ignoreCase) {
        LList result = LList.Empty;
        Pair lastPair = null;
        while (rest instanceof Pair) {
            Pair pair = (Pair) rest;
            Object paircar = pair.getCar();
            Object savePos1 = tr.pushPositionOf(pair);
            if (! (paircar instanceof CharSequence)) {
                tr.error('e', "include parameters must be strings");
            }
            String fname = paircar.toString();
            Path path;
            BinaryInPort inp;
            Iterator<CharSequence> searchIterator =
                getIncludeSearchPath().iterator();
            CharSequence searchElement = relative ? "|" : null;
            for (;; searchElement = null) {
                if (searchElement == null) {
                    if (! searchIterator.hasNext()) {
                        tr.error('e', "cannot open file \""+fname+"\"");
                        return result;
                    }
                    searchElement = searchIterator.next();
                }
                Path pathElement;
                if (searchElement.length() > 0 && searchElement.charAt(0) == '|') {
                    pathElement = tr.getMinfo().getSourceAbsPath();
                    boolean currentIsFile = pathElement instanceof FilePath
                        && ((FilePath) pathElement).toFileRaw().isFile();
                    if (! currentIsFile)
                        pathElement = Path.currentPath();
                    if (searchElement.length() > 1)
                        pathElement = pathElement.resolve(searchElement.toString()
                                                          .substring(1));
                }
                else
                    pathElement = Path.valueOf(searchElement);
                try {
                    path = pathElement.resolve(fname);
                    InputStream istrm = path.openInputStream();
                    try {
                        inp = BinaryInPort.openHeuristicFile(istrm, path);
                    } catch (Exception ex) {
                        tr.error('e', ("error reading file \""+path
                                       +"\": "+ex.getMessage()));
                        return result;
                    }
                    break;
                } catch (Exception ex) {
                }
            }
            
            tr.popPositionOf(savePos1);
            LispReader reader = new LispReader(inp, tr.getMessages());
            if (ignoreCase)
                reader.setReadCase('D');
            Lexer saveLexer = tr.lexer;
            tr.lexer = reader;
            try {
                if (inp.getCharset() == null && saveLexer != null) {
                    InPort savePort = saveLexer.getPort();
                    if (savePort instanceof BinaryInPort) {
                        Charset saveCset =
                            ((BinaryInPort) savePort).getCharset();
                        if (saveCset != null)
                            inp.setDefaultCharset(saveCset);
                    }
                }
                for (;;) {
                    Object sexp;
                    try {
                        sexp = reader.readCommand();
                        if (sexp == Sequence.eofValue)
                            break;
                    } catch (Exception ex) {
                        tr.error('e', "error reading file \""+path+"\": "+ex.getMessage());
                        return result;
                    }
                    // FIXME do we need to handle Syntax context?
                    if (defs != null) {
                        // In scan context.
                        tr.scanForm(sexp, defs);
                    } else {
                        // In rewrite context - create list.
                        Pair npair = new Pair(sexp, LList.Empty);
                        if (lastPair == null)
                            result = npair;
                        else
                            lastPair.setCdrBackdoor(npair);
                        lastPair = npair;
                    }
                }
            } finally {
                tr.lexer = saveLexer;
            }
            rest = pair.getCdr();
        }
        if (rest != LList.Empty)
            tr.error('e', "improper list");
        return result;
    }

    public static final ThreadLocal<List<CharSequence>> searchPath
        = new InheritableThreadLocal<List<CharSequence>>();
    public static List<CharSequence> getIncludeSearchPath() {
        return getSearchPath(searchPath, "kawa.include.path", "|:.");
    }
   
    public static List<CharSequence> getSearchPath(ThreadLocal<List<CharSequence>> var,
                                                   String propertyName,
                                                   String defaultPath) {
        List<CharSequence> path = var.get();
        if (path != null)
            return path;
        String pstr = System.getProperty(propertyName);
        if (pstr == null) {
            if (defaultPath == null)
                return null;
            pstr = defaultPath;
        }
        StringTokenizer tokenizer =
            new StringTokenizer(pstr, File.pathSeparator);
        path = new ArrayList<CharSequence>();
        while (tokenizer.hasMoreTokens()) {
            String str = tokenizer.nextToken().trim();
            if (str.length() > 0)
                path.add(str);
        }
        var.set(path);
        return path;
    }
    
}
