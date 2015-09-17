// Copyright (c) 2013  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.functions;

import gnu.expr.Keyword;
import gnu.kawa.io.BinaryInPort;
import gnu.kawa.io.BinaryOutPort;
import gnu.kawa.io.FilePath;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.lists.ByteVector;
import gnu.lists.Consumer;
import gnu.lists.Strings;
import gnu.mapping.*;
import gnu.text.DelimitSubstitutionFormat;
import java.io.*;
import java.util.*;
import java.util.Map;
/* #ifdef JAVA7 */
import java.lang.ProcessBuilder.Redirect;
/* #endif */

/** The Kawa run-process command builds and runs a Process.
 */

public class RunProcess extends MethodProc {

    public static final RunProcess instance = new RunProcess("run-process");

    public RunProcess(String name) {
        setName(name);
        setProperty(Procedure.validateApplyKey,
                    "gnu.kawa.functions.CompileProcess:validateApplyRunProcess");
    }

    public void apply (CallContext ctx) throws Throwable {
        doit(ctx.getArgs(), ctx.consumer);
    }

    protected void error(String message) {
        throw new RuntimeException("run-process: "+message);
    }

    public static final SimpleSymbol inheritSymbol = Symbol.valueOf("inherit");
    public static final SimpleSymbol pipeSymbol = Symbol.valueOf("pipe");
    public static final SimpleSymbol currentSymbol = Symbol.valueOf("current");
    public static final SimpleSymbol outSymbol = Symbol.valueOf("out");

    public void doit(Object[] args, Consumer consumer) throws Throwable {
        ProcessBuilder builder = new ProcessBuilder();
        int nargs = args.length;
        boolean useShell = false;
        boolean returnBlob = true;
        Object inRedirect = null;
        Object outRedirect = null;
        Object errRedirect = null;
        boolean outNeedsClose = false;
        boolean errNeedsClose = false;
        InputStream inputBytes = null;
        boolean directorySet = false;
        Object command = null;
        for (int iarg = 0; iarg < nargs;  iarg++) {
            Object arg = args[iarg];
            if (arg instanceof Keyword) {
                String key = ((Keyword) arg).getName();
                boolean outSpecifier = key.startsWith("out");
                if (++iarg >= nargs)
                    error("missing keyword value for keyword "+arg);
                Object kval = args[iarg];
                Object newRedirect = null;
                if (key.equals("shell")) {
                    useShell = ((Boolean) kval).booleanValue();
                } else if (key.equals("in")) {
                    inputBytes = getInputStreamFrom(kval);
                }
                else if (key.equals("out-to")
                           || key.equals("err-to")
                           || key.equals("in-from")) {
                    boolean inSpecifier = key.equals("in-from");
                    if (kval == currentSymbol) {
                        newRedirect = inSpecifier ? InPort.inDefault()
                            : outSpecifier ? OutPort.outDefault()
                            : OutPort.errDefault();
                    }
                    /* #ifdef JAVA7 */
                    else if (kval == inheritSymbol) {
                        newRedirect = Redirect.INHERIT;
                    }
                    else if (kval == pipeSymbol) {
                        newRedirect = Redirect.PIPE;
                    }
                    /* #else */
                    // else if (kval == pipeSymbol) {
                    //     newRedirect = kval;
                    // }
                    // else if (kval == inheritSymbol && ! inSpecifier) {
                    //     newRedirect = outSpecifier ? OutPort.getSystemOut()
                    //         : OutPort.getSystemErr();
                    // }
                    /* #endif */
                    else if (! outSpecifier && ! inSpecifier
                             && kval == outSymbol) { // i.e. err-to: 'out
                        /* #ifdef JAVA7 */
                        builder.redirectErrorStream(true);
                        /* #else */
                        // error("err-to: 'out redirect requires Java 7");
                        // newRedirect = null;
                        /* #endif */
                    } else if (inSpecifier
                               ? (kval instanceof InputStream
                                  || kval instanceof Reader)
                               : (kval instanceof OutputStream
                                  || kval instanceof Writer)) {
                         newRedirect = kval;
                    }  else {
                        FilePath fpath = FilePath.coerceToFilePathOrNull(kval);
                        if (fpath != null) {
                            File file = fpath.toFile();
                            /* #ifdef JAVA7 */
                            newRedirect = inSpecifier ? Redirect.from(file)
                                     : Redirect.to(file);
                            /* #else */
                            // if (inSpecifier) {
                            //     inputBytes = new FileInputStream(file);
                            // } else {
                            //     newRedirect = new FileOutputStream(file, false);
                            //     if (outSpecifier)
                            //         outNeedsClose = true;
                            //     else
                            //         errNeedsClose = true;
                            // }
                            /* #endif */
                        } else
                            error("unimplemented keyword value for "+arg);
                    }
                    if (inSpecifier) {
                        inRedirect = newRedirect;
                        newRedirect = null;
                    }
                }
                else if (key.equals("out-append-to")
                         || key.equals("err-append-to")) {
                    FilePath fpath = FilePath.coerceToFilePathOrNull(kval);
                    if (fpath != null) {
                        File file = fpath.toFile();
                        /* #ifdef JAVA7 */
                            newRedirect =
                                ProcessBuilder.Redirect.appendTo(file);
                        /* #else */
                        // newRedirect = new FileOutputStream(file, true);
                        /* #endif */
                    } else
                        error("unimplemented keyword value for "+arg);
                 }
                else if (key.startsWith("env-") && key.length() > 0) {
                    String evar = key.substring(4);
                    String evalue = kval.toString();
                    builder.environment().put(evar, evalue);
                } else if (key.toUpperCase().equals(key)) {
                    String evalue = kval.toString();
                    builder.environment().put(key, evalue);
                } else if (key.equals("env")) {
                    Map<String,String> env = builder.environment();
                    env.clear();
                    env.putAll((Map<String,String>) kval);
                } else if (key.equals("directory")) {
                    try {
                        directorySet = true;
                        if (kval != inheritSymbol) {
                            FilePath fpath =
                                FilePath.coerceToFilePathOrNull(kval);
                            builder.directory(fpath.toFile());
                        }
                    } catch (Exception ex) {
                        throw new IllegalArgumentException("invalid directory");
                    }
                } else
                    error("unknown keyword "+arg);
                if (outSpecifier)
                    returnBlob = false;
                if (newRedirect != null) {
                    /* MAYBE error if duplicate redirect? */
                    if (outSpecifier) {
                        outRedirect = newRedirect;
                    }
                    else {
                        errRedirect = newRedirect;
                    }
                }
            } else {
                if (inputBytes == null && iarg+2==nargs)
                    inputBytes = getInputStreamFrom(arg);
                else if (command == null)
                    command = arg;
                else
                    error("multiple command arguments");
            }
        }
        List<String> cmd = null;
        if (command instanceof CharSequence)
            ; // We'll tokenize it below
        else if (command instanceof List) {
            cmd = new ArrayList<String>();
            for (Object arg : (List) command) {
                if (arg instanceof CharSequence)
                    cmd.add(arg.toString());
                else
                    error("element in command sequence is not a string");
            }
            if (cmd.isEmpty())
                command = null;
        } else
            error("command is neither string nor string sequence");
        if (command == null)
            error("missing command");
        if (useShell) {
            if (cmd != null) {
                StringBuilder sbuf = new StringBuilder(cmd.get(0));
                int ncmds = cmd.size();
                for (int i = 1;  i < ncmds;  i++) {
                    sbuf.append(' ');
                    sbuf.append(cmd.get(i));
                }
                command = sbuf;
            }
            cmd = new ArrayList<String>();
            cmd.add("/bin/sh");
            cmd.add("-c");
            String commands = command.toString();
            tokenize(commands, true, cmd);
            if (false) { // DEBUGGING
                System.err.print("tokenize-with-shell: ");
                Strings.printQuoted(commands, System.err, 2);
                System.err.print(" -> ");
                Strings.printQuoted(cmd.get(2), System.err, 2);
                System.err.println();
            }
        } else {
            if (cmd == null) {
                cmd = new ArrayList<String>();
                String commands = command.toString();
                tokenize(commands, false, cmd);
                if (false) { // DEBUGGING
                    System.err.print("tokenize: ");
                    Strings.printQuoted(commands, System.err, 2);
                    System.err.print(" ->");
                    for (String c : cmd) {
                        System.err.print(' ');
                        Strings.printQuoted(c, System.err, 2);
                    }
                    System.err.println();
                }
            }
        }
        builder.command(cmd);
        if (! directorySet) {
             Path cur = Path.currentPath();
             if (cur != Path.userDirPath)
                 builder.directory(((FilePath) cur).toFile());
        }
        /* #ifdef JAVA7 */
        if (inRedirect instanceof ProcessBuilder.Redirect) {
            builder.redirectInput((ProcessBuilder.Redirect)inRedirect);
        }
        /* #else */
        /* #endif */
        if (errRedirect == null) {
            errRedirect = OutPort.errDefault();
        }
        /* #ifdef JAVA7 */
        if (outRedirect == OutPort.getSystemOut()) {
            outRedirect = Redirect.INHERIT;
        }
        if (errRedirect == OutPort.getSystemErr()) {
            errRedirect = Redirect.INHERIT;
        }
        if (outRedirect instanceof ProcessBuilder.Redirect)
            builder.redirectOutput((ProcessBuilder.Redirect)outRedirect);
        if (errRedirect instanceof ProcessBuilder.Redirect)
            builder.redirectError((ProcessBuilder.Redirect)errRedirect);
        /* #endif */
        final Process proc = builder.start();
        if (inRedirect instanceof Reader) {
            if (inRedirect instanceof BinaryInPort) {
                inputBytes = ((BinaryInPort) inRedirect).getInputStream();
            } else {
                OutputStreamWriter outs 
                    = new OutputStreamWriter(proc.getOutputStream());
                copyCharsInThread((Reader) inRedirect, outs, false, true);
            }
        }
        if (inRedirect instanceof InputStream) {
            inputBytes = (InputStream) inRedirect;
        }
        if (inputBytes != null) {
            final InputStream inb = inputBytes;
            copyStreamInThread(inputBytes, proc.getOutputStream(), true);
        }
        if (outRedirect instanceof OutputStream) {
            copyStreamInThread(proc.getInputStream(),
                               (OutputStream) outRedirect, outNeedsClose);
        } else if (outRedirect instanceof Writer) {
            copyWriterInThread(proc.getInputStream(),
                                (Writer) outRedirect, outNeedsClose);
        }
        if (errRedirect instanceof OutputStream) {
            copyStreamInThread(proc.getErrorStream(),
                               (OutputStream) errRedirect, errNeedsClose);
        } else if (errRedirect instanceof Writer) {
            copyWriterInThread(proc.getErrorStream(),
                                (Writer) errRedirect, errNeedsClose);
        }
        if (returnBlob) {
            LProcess lproc = new LProcess(proc);
            if (consumer instanceof OutPort
                && isDisplayConsumer(consumer)) {
                InputStream in = proc.getInputStream();
                if (consumer instanceof BinaryOutPort) {
                    BinaryOutPort bout = (BinaryOutPort) consumer;
                    byte[] buffer = new byte[2048];
                    for (;;) {
                        int cnt = in.read(buffer, 0, buffer.length);
                        if (cnt < 0)
                            break;
                        bout.writeBytes(buffer, 0, cnt);
                        bout.flush();
                    }
                    in.close();
                }
                else {
                    // InputStreamReader automatically buffers the InputStream.
                    Reader inr = new InputStreamReader(in);
                    char[] buffer = new char[2048];
                    for (;;) {
                        int cnt = inr.read(buffer, 0, buffer.length);
                        if (cnt < 0)
                            break;
                        consumer.write(buffer, 0, cnt);
                        ((OutPort) consumer).flush();
                    }
                    inr.close();
                }
            }
            else
                consumer.writeObject(lproc);
        }
        else {
            consumer.writeObject(proc);
        }
    }

    public boolean isDisplayConsumer(Consumer out) {
        if (out instanceof OutPort) {
            OutPort outp = (OutPort) out;
            if (outp.objectFormat instanceof DisplayFormat) {
                return ! ((DisplayFormat) outp.objectFormat)
                    .getReadableOutput();
            }
        }
        return false;
    }

    /** Parse strings into token, handling substitution marks.
     * @param useShell true if result will be further tokenized by a shell.
     *   (In this case we're basically just handling substiution marks.)
     */
    public void tokenize(String str, boolean useShell, List<String> arr) {
        // The buffer for building the current command-line argument.
        StringBuffer sbuf = new StringBuffer(100);
        // The default is state==-1.
        // If state is '"' or '\'' then we're inside a quoted string.
        // If state==1 (only if useShell) then we're inside added '\''
        // that aren't in the input string.
        // If state==0 (only if !useShell), we have seen a quoted
        // possibly-empty string, while state==-1 if we haven't seen a 
        // quoted string this arg. The distinction matters because ""
        // should create an argument, so we can't just check
        // sbuf.length()>0 in deciding that we have a new argument;
        // we also check if state >= 0.
        int state = -1;
        int len = str.length();
        int inGroup = 0;
        int inSubstitution = 0;
        for (int i = 0;  i < len; i++) {
            char ch = str.charAt(i);
            if (ch == DelimitSubstitutionFormat.MARK_GROUP_START) {
                if (inGroup > 0)
                    sbuf.append(ch);
                inGroup++;
                continue;
            }
            if (ch == DelimitSubstitutionFormat.MARK_GROUP_END) {
                inGroup--;
                if (inGroup > 0)
                    sbuf.append(ch);
                continue;
            }
            if (ch == DelimitSubstitutionFormat.MARK_SUBSTITUTION_START) {
                if (inSubstitution > 0)
                    sbuf.append(ch);
                inSubstitution++;
                continue;
            }
            if (ch == DelimitSubstitutionFormat.MARK_SUBSTITUTION_END) {
                inSubstitution--;
                if (inSubstitution > 0)
                    sbuf.append(ch);
                else {
                    if (state == 1) {
                        sbuf.append('\'');
                        state = -1;
                    }
                    if (inGroup > 0 && i+1 < len
                        && str.charAt(i+1) == DelimitSubstitutionFormat.MARK_SUBSTITUTION_START) {
                        if (useShell || state == '\"' || state == '\'') {
                            sbuf.append(' '); // IFS
                        } else {
                            arr.add(sbuf.toString());
                            sbuf.setLength(0);
                        }
                    }
                }
                continue;
            }
            
            if (ch == '\n' && inSubstitution > 0 && inGroup == 0) {
                // Check for final newline(s) in substitution.
                int nlCount = 1;  // Count of '\n'
                for (;;) {
                    ch = str.charAt(i+nlCount);
                    if (ch != '\n')
                        break;
                    nlCount++;
                }
                i += nlCount-1;
                if (ch == DelimitSubstitutionFormat.MARK_SUBSTITUTION_END) {
                    // We saw nlCount final newlines.  Skip them,
                    continue;
                }
                ch = '\n';
                if (state == '"' && inGroup == 0) {
                    // In '"' '\n' is a separator.
                    while (--nlCount >= 0) {
                        if (useShell) {
                            sbuf.append("\" \"");
                        } else {
                            arr.add(sbuf.toString());
                            sbuf.setLength(0);
                        }
                    }
                    continue;
                } else if (state <= 1) {
                    // Token separator - handled below
                    if (useShell)
                        ch = ' ';
                } else {
                    while (--nlCount > 0) {
                        sbuf.append('\n');
                    }
                }
            }
            if (useShell) {
                if (inSubstitution > 0) {
                    // May need to quote special characters
                    if (state == '"') {
                        if (ch == '$' || ch == '\\')
                            sbuf.append('\\');
                    } else if (ch == '\'') {
                        if (state == -1)
                            sbuf.append("\\'");
                        else // state is ' or state is 1
                            sbuf.append("'\\'"); // Another '"' added below
                    } else if (state <= 1 && inGroup == 0
                               && (ch == ' ' || ch == '\t'
                                   || ch == '\n' || ch == '\r')) {
                        if (state == 1)
                            sbuf.append('\'');
                        state = -1;
                    } else if (state == -1) {
                        sbuf.append('\'');
                        state = 1;
                    }
                }
                else {
                    if (ch == '\\' && state != '\'' && i+1 < len) {
                        sbuf.append(ch);
                        i++;
                        ch = str.charAt(i);
                    } else if (state < 0) {
                        if (ch == '\"' || ch == '\'')
                            state = ch;
                    }
                    else if (ch == state) {
                        state = -1;
                    }
                }
            }
            else { // !useShell
                if (state <= 0 && inGroup == 0
                    && (ch == ' ' || ch == '\t'
                        || ch == '\n' || ch == '\r')) {
                    if (sbuf.length() > 0 || state == 0) {
                        arr.add(sbuf.toString());
                        sbuf.setLength(0);
                        state = -1;
                    }
                    continue;
                } else if (inSubstitution > 0) {
                }
                else if (state <= 0) {
                    if (ch == '\\' || ch == '\'' || ch == '\"') {
                        state = ch;
                        continue;
                    }
                } else if (state == '\\')
                    state = 0;
                else if (ch == state) {
                    state = 0;
                    continue;
                }
            }
            sbuf.append(ch);
        }
        if (sbuf.length() > 0 || state >= 0 || useShell)
            arr.add(sbuf.toString());
        if (! useShell && (state > 0 || inSubstitution > 0 || inGroup > 0))
            error("bad quotes");
    }

    public static InputStream getInputStreamFrom(Object val) {
        if (val instanceof ByteVector)
            return ((ByteVector) val).getInputStream();
        if (val instanceof Process)
            return ((Process) val).getInputStream();
        if (val instanceof byte[])
            return new ByteArrayInputStream((byte[]) val);
        if (val instanceof CharSequence)
            // FIXME should be able to override Charset
            // FIXME can perhaps optimize for CharSeq by using writeTo.
            return new ByteArrayInputStream(((CharSequence) val).toString().getBytes());
        throw new ClassCastException("invalid input");
    }

    /** Copy bytes from InputStream to OutputStream using separate Thread.
     * Continue copying until EOF or exception.
     * At end, the input stream is closed, but the output stream is not.
     */
    static void copyStreamInThread(final InputStream in,
                                   final OutputStream out,
                                   final boolean closeOut) {
        Thread thread = 
            new Thread() {
                public void run() {
                    try {
                        copyStream(in, out, closeOut);
                    } catch (IOException ex) {
                        // FIXME - should do better
                        throw new RuntimeException(ex);
                    }
                }
            };
        thread.start();
    }

    /** Copy bytes from InputStream to a Writer using separate thread.
     * The InputStream is wrapped in a InputStreamWriter, unless
     * the Writer is a BinaryOutPort (in which case bytes are copied).
     * Continue copying until EOF or exception.
     * At end, the InputStream is closed, but the Writer is not.
     */
    void copyWriterInThread(final InputStream in,
                            final Writer out, boolean closeOut) throws IOException {
        if (out instanceof BinaryOutPort) {
            BinaryOutPort bout = (BinaryOutPort) out;
            copyStreamInThread(in, bout.getOutputStream(), closeOut);
        } else {
            out.flush();
            copyCharsInThread(new InputStreamReader(in), out,
                              true, closeOut);
        }
    }

    static void copyCharsInThread(final Reader in, final Writer out,
                                  final boolean closeIn,
                                  final boolean closeOut)
            throws IOException {
        (new Thread() {
            public void run() {
                try {
                    char[] buffer = new char[2048];
                    try {
                        for (;;) {
                            try { 
                                int cnt = in.read(buffer, 0, buffer.length);
                                if (cnt < 0)
                                    break;
                                out.write(buffer, 0, cnt);
                            } catch (IOException ex) {
                                if ("Broken pipe".equals(ex.getMessage())) {
                                    break;
                                }
                                throw ex;
                            }
                        }
                        out.flush();
                    } finally {
                        in.close();
                        if (closeOut)
                            out.close();
                    }
                } catch (IOException ex) {
                    throw new RuntimeException(ex); // ???
                }
            }}).start();
    }

    /** Copy bytes from InputStream to OutputStream using current Thread.
     * Continue copying until EOF or exception.
     * At end, the input stream is closed, but the output stream is not.
     */
    public static void copyStream(InputStream in,
                           OutputStream out, boolean closeOut)
            throws IOException {
        byte[] buffer = new byte[2048];
        try {
            for (;;) {
                int cnt = in.read(buffer, 0, buffer.length);
                if (cnt < 0)
                    break;
                try {
                    out.write(buffer, 0, cnt);
                } catch (IOException ex) {
                    if ("Broken pipe".equals(ex.getMessage())) {
                        break;
                    }
                    throw ex;
                }
            }
            out.flush();
        } finally {
            in.close();
            if (closeOut)
                out.close();
        }
    }
}
