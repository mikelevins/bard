package kawa;
import java.io.*;
import java.net.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import gnu.lists.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import gnu.bytecode.ClassType;
import gnu.kawa.servlet.HttpRequestContext;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.io.WriterManager;
import gnu.kawa.util.ExitCalled;
import kawa.lang.SyntaxPattern;

/** Start a "Read-Eval-Print-Loop" for the Kawa Scheme evaluator. */

public class repl extends Procedure0or1
{
  public static String compilationTopname = null;

  Language language;
  static Language previousLanguage;

  static int defaultParseOptions = Language.PARSE_PROLOG|Language.PARSE_EXPLICIT;

  public repl(Language language)
  {
    this.language = language;
  }

  public Object apply0 ()
  {
    Shell.run(language, Environment.getCurrent());
    return Values.empty;
  }

  public Object apply1(Object env)
  {
    Shell.run(language, (Environment) env);
    return Values.empty;
  }

  static void bad_option (String str)
  {
    System.err.println ("kawa: bad option '" + str + "'");
    printOptions(System.err);
    System.exit (-1);
  }

  public static void printOption(PrintStream out, String option, String doc)
  {
    out.print(" ");
    out.print(option);

    int len = option.length() + 1;
    for (int i = 0; i < 30 - len; ++i)
      out.print(" ");
    out.print(" ");
    out.println(doc);
  }
  
  public static void printOptions(PrintStream out)
  {
    out.println("Usage: [java kawa.repl | kawa] [options ...]");
    out.println();
    out.println(" Generic options:");
    printOption(out, "--help", "Show help about options");
    printOption(out, "--author", "Show author information");
    printOption(out, "--version", "Show version information");
    out.println();
    out.println(" Options");
    printOption(out, "-e <expr>", "Evaluate expression <expr>");
    printOption(out, "-c <expr>", "Same as -e, but make sure ~/.kawarc.scm is run first");
    printOption(out, "-f <filename>", "File to interpret");
    printOption(out, "-s| --", "Start reading commands interactively from console");
    printOption(out, "-w", "Launch the interpreter in a GUI window");
    printOption(out, "--server <port>", "Start a server accepting telnet connections on <port>");
    printOption(out, "--debug-dump-zip", "Compiled interactive expressions to a zip archive");
    printOption(out, "--debug-print-expr", "Print generated internal expressions");
    printOption(out, "--debug-print-final-expr", "Print expression after any optimizations");
    printOption(out, "--debug-error-prints-stack-trace", "Print stack trace with errors");
    printOption(out, "--debug-warning-prints-stack-trace", "Print stack trace with warnings");
    printOption(out,"--[no-]full-tailcalls", "(Don't) use full tail-calls");
    printOption(out, "-C <filename> ...", "Compile named files to Java class files");
    printOption(out, "--output-format <format>", "Use <format> when printing top-level output");
    printOption(out,"--<language>", "Select source language, one of:");
    String[][] languages = Language.getLanguages();
    for (int i = 0; i < languages.length; i++)
      {
	out.print("   ");
	String[] lang = languages[i];
	// skip last entry, which is class name
	int nwords = lang.length - 1;
	for (int j = 0; j < nwords; j++) 
	  out.print(lang[j] + " ");
	if (i == 0)
	  out.print("[default]");
	out.println();
      }
    out.println(" Compilation options, must be specified before -C");
    printOption(out, "-d <dirname>", "Directory to place .class files in");
    printOption(out, "-P <prefix>", "Prefix to prepand to class names");
    printOption(out, "-T <topname>", "name to give to top-level class");
    
    printOption(out, "--applet", "Generate an applet");
    printOption(out, "--servlet", "Generate a servlet");
    printOption(out, "--module-static", "Top-level definitions are by default static");

    List<String> keys = Compilation.options.keys();
    Collections.sort(keys);
    for (int i = 0; i < keys.size(); ++i)
      {
        String name = keys.get(i);
        printOption(out, "--" + name, Compilation.options.getDoc(name));
      }
        
    out.println();
    out.println("For more information go to:  http://www.gnu.org/software/kawa/");
  }

  public static String homeDirectory;

  static void checkInitFile ()
  {
    /* Set homeDirectory;  if first time called, run ~/.kawarc.scm. */
    if (homeDirectory == null)
      {
	File initFile = getInitFile();
	if (initFile != null && initFile.exists())
	  if (! Shell.runFileOrClass(initFile.getPath(), true, 0))
	    System.exit(-1);
      }
  }

  static File getInitFile()
  {
    File initFile = null;
    homeDirectory = System.getProperty ("user.home");
    Object scmHomeDirectory;
    if (homeDirectory != null)
      {
	scmHomeDirectory = new FString (homeDirectory);
	String file_separator = System.getProperty("file.separator");
	String kawarc_name = new String();
	if (Language.getDefaultLanguage().getName().equals("Emacs-Lisp"))
	    kawarc_name = ".jemacs";
	else
	    kawarc_name =
	  "/".equals(file_separator) ? ".kawarc.scm"
	  : "kawarc.scm";
	initFile = new File(homeDirectory, kawarc_name);
      }
    else
      scmHomeDirectory = Boolean.FALSE;
    Environment.getCurrent().put("home-directory", scmHomeDirectory);
    return initFile;
  }

  /** Evaluate init file, if any, returning error message on failure. */
  public static String evalInitFileWithErrorMessage ()
  {
    File initFile = getInitFile();
    if (initFile != null && initFile.exists())
      {
        try 
          {
            Path path = Path.valueOf(initFile.getPath());
            InputStream fs = path.openInputStream();
            Environment env = Environment.getCurrent();
            Shell.runFile(fs, path, env, true, 0);
          }
        catch (Error e)
          {
            throw e;
          }
        catch (Throwable e)
          {
            return "An error occurred while loading '" + initFile +"' : " + e;
          }
      }
    return null;
  }

  public static void setArgs (String[] args, int arg_start)
  {
    ApplicationMainSupport.setArgs(args, arg_start);
  }

  public static void getLanguageFromFilenameExtension(String name)
  {
    if (previousLanguage == null)
      {
	previousLanguage = Language.getInstanceFromFilenameExtension(name);
	if (previousLanguage != null)
	  {
	    Language.setDefaults(previousLanguage);
	    return;
	  }
      }
    getLanguage();
  }

  public static void getLanguage()
  {
    if (previousLanguage == null)
      {
	previousLanguage = Language.getInstance(null);
	Language.setDefaults(previousLanguage);
      }
  }

  static boolean shutdownRegistered
    = WriterManager.instance.registerShutdownHook();

  public static int processArgs(String[] args, int iArg, int maxArg)
  {
    boolean something_done = false;
    int returnDelta = 0;
    for ( ;  iArg < maxArg;  iArg++)
      {
	String arg = args[iArg];
	if (arg.equals ("-c") || arg.equals ("-e"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    getLanguage();
	    setArgs (args, iArg+1);
	    if (arg.equals ("-c"))
	      checkInitFile();
	    Language language = Language.getDefaultLanguage();
            SourceMessages messages = new SourceMessages();
            Throwable ex = Shell.run(language, Environment.getCurrent(),
                                     new CharArrayInPort(args[iArg]),
                                     OutPort.outDefault(),
                                     null, messages);
            if (ex != null)
              {
                Shell.printError(ex, messages, OutPort.errDefault());
                System.exit(-1);
              }
	    something_done = true;
	  }
	else if (arg.equals ("-f"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    String filename = args[iArg];
	    getLanguageFromFilenameExtension(filename);
	    setArgs (args, iArg+1);
	    checkInitFile();
	    if (! Shell.runFileOrClass(filename, true, 0))
              System.exit(-1);
	    something_done = true;
	  }
	else if (arg.startsWith("--script"))
	  {
            String count = arg.substring(8);
	    iArg++;
            int skipLines = 0;
            if (count.length() > 0)
              {
                try
                  {
                    skipLines = Integer.parseInt(count);
                  }
                catch (Exception ex)
                  {
                    iArg = maxArg; // force bad_option.
                  }
              }
	    if (iArg == maxArg)
	      bad_option (arg);
	    String filename = args[iArg];
	    getLanguageFromFilenameExtension(filename);
	    setArgs (args, iArg+1);
	    checkInitFile();
	    if (! Shell.runFileOrClass(filename, true, skipLines))
              System.exit(-1);
            return -1;
	  }
	else if (arg.equals("\\"))
	  {
	    // Scsh-like "meta-arg".  See Kawa manual (SOON-FIXME).
	    if (++iArg == maxArg)
	      bad_option (arg);
	    String filename = args[iArg];
            ApplicationMainSupport.commandName.set(filename);
	    InPort freader;
            SourceMessages messages = new SourceMessages();
	    try
	      {
		InputStream fstream = new BufferedInputStream(new FileInputStream(filename));
		int ch = fstream.read();
		if (ch == '#')
		  {
		    StringBuffer sbuf = new StringBuffer(100);
		    ArrayList<String> xargs = new ArrayList<String>(10);
		    int state = 0;
		    while (ch != '\n' && ch != '\r' && ch >= 0)
		      ch = fstream.read();
		    for (;;)
		      {
			ch = fstream.read();
			if (ch < 0)
			  {
			    System.err.println("unexpected end-of-file processing argument line for: '" + filename + '\'');
			    System.exit(-1);
			  }
			if (state == 0)
			  {
			    if (ch == '\\' || ch == '\'' || ch == '\"')
			      {
				state = ch;
				continue;
			      }
			    else if (ch == '\n' || ch == '\r')
			      break;
			    else if (ch == ' ' || ch == '\t')
			      {
				if (sbuf.length() > 0)
				  {
				    xargs.add(sbuf.toString());
				    sbuf.setLength(0);
				  }
				continue;
			      }
			  }
			else if (state == '\\')
			  state = 0;
			else if (ch == state)
			  {
			    state = 0;
			    continue;
			  }
			sbuf.append((char) ch);
		      }
		    if (sbuf.length() > 0)
		      xargs.add(sbuf.toString());
		    int nxargs = xargs.size();
                    iArg--;
                    String[] nargs = new String[maxArg+nxargs-1];
                    System.arraycopy(args, 0, nargs, 0, iArg);
                    for (int i = 0;  i < nxargs;  i++)
                        nargs[iArg+i] = xargs.get(i);
                    System.arraycopy(args, iArg+1, nargs, iArg+nxargs,
                                     maxArg-iArg-1);
                    maxArg = maxArg+nxargs-1;
                    returnDelta += nargs.length-args.length;
                    args = nargs;
                    iArg--;
                    continue;
		  }
	      }
	    catch (Throwable ex)
	      {
                Shell.printError(ex, messages,  OutPort.errDefault());
		System.exit(1);
	      }
	    return -1;
	  }
	else if (arg.equals ("-s") || arg.equals ("--"))
	  {
	    iArg++;
	    getLanguage();
	    setArgs (args, iArg);
	    checkInitFile();
	    Shell.run(Language.getDefaultLanguage(), Environment.getCurrent());
	    return -1;
	  }
	else if (arg.equals ("-w"))
	  {
	    iArg++;
	    getLanguage();
	    setArgs (args, iArg);
	    checkInitFile();
            startGuiConsole();
	    something_done = true;
	  }
	else if (arg.equals ("-d"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
            ModuleManager manager = ModuleManager.getInstance();
	    manager.setCompilationDirectory(args[iArg]);
	  }
        else if (arg.equals("--target") || arg.equals("-target"))
          {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
            arg = args[iArg];
            if (arg.equals("8") || arg.equals("1.8"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_8_VERSION;
            else if (arg.equals("7") || arg.equals("1.7"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_7_VERSION;
            else if (arg.equals("6") || arg.equals("1.6"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_6_VERSION;
            else if (arg.equals("5") || arg.equals("1.5"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_5_VERSION;
            else if (arg.equals("1.4"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_4_VERSION;
            else if (arg.equals("1.3"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_3_VERSION;
            else if (arg.equals("1.2"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_2_VERSION;
            else if (arg.equals("1.1"))
              Compilation.defaultClassFileVersion = ClassType.JDK_1_1_VERSION;
            else
              bad_option(arg);
          }
	else if (arg.equals ("-P"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    Compilation.classPrefixDefault = args[iArg];
	  }
	else if (arg.equals ("-T"))
	  {
	    iArg++;
	    if (iArg == maxArg)
	      bad_option (arg);
	    compilationTopname = args[iArg];
	  }
        else if (arg.equals ("--main"))
          {
	    defaultParseOptions |= Language.PARSE_EMIT_MAIN;
          }
	else if (arg.equals ("-C"))
	  {
	    ++iArg;
	    if (iArg == maxArg)
	      bad_option (arg);
            compileFiles(args, iArg, maxArg);
	    return -1;
	  }
	else if (arg.equals("--output-format")
		 || arg.equals("--format"))
	  {
	    if (++iArg == maxArg)
	      bad_option (arg);
	    Shell.setDefaultFormat(args[iArg]);
	  }
	else if (arg.equals("--connect"))
	  {
	    ++iArg;
	    if (iArg == maxArg)
	      bad_option (arg);
	    int port;
	    if (args[iArg].equals("-"))
	      port = 0;
	    else
	      {
		try
		  {
		    port = Integer.parseInt(args[iArg]);
		  }
		catch (NumberFormatException ex)
		  {
		    bad_option ("--connect port#");
		    port = -1; // never seen.
		  }
	      }
	    try
	      {
		Socket socket = new Socket(InetAddress.getByName(null), port);
		Telnet conn = new Telnet(socket, true);
		java.io.InputStream sin = conn.getInputStream();
		java.io.OutputStream sout = conn.getOutputStream();
		java.io.PrintStream pout = new PrintStream (sout, true);
		System.setIn(sin);
		System.setOut(pout);
		System.setErr(pout);
	      }
	    catch (java.io.IOException ex)
	      {
		ex.printStackTrace(System.err);
		throw new Error(ex.toString());
	      }
	  }
	else if (arg.equals("--server"))
	  {
	    getLanguage();
	    ++iArg;
	    if (iArg == maxArg)
	      bad_option (arg);
	    int port;
	    if (args[iArg].equals("-"))
	      port = 0;
	    else
	      {
		try
		  {
		    port = Integer.parseInt(args[iArg]);
		  }
		catch (NumberFormatException ex)
		  {
		    bad_option ("--server port#");
		    port = -1; // never seen.
		  }
	      }
	    try
	      {
		java.net.ServerSocket ssocket
		  = new java.net.ServerSocket(port);
		port = ssocket.getLocalPort();
		System.err.println("Listening on port "+port);
		for (;;)
		  {
		    System.err.print("waiting ... ");  System.err.flush();
		    java.net.Socket client = ssocket.accept();
		    System.err.println("got connection from "
				       +client.getInetAddress()
				       +" port:"+client.getPort());
		    TelnetRepl.serve(Language.getDefaultLanguage(), client);
		  }
	      }
	    catch (java.io.IOException ex)
	      {
		throw new Error(ex.toString());
	      }
	  }
        else if (arg.equals("--http-auto-handler"))
          {
            iArg += 2;
	    if (iArg >= maxArg)
	      bad_option (arg);
            /* #ifdef use:com.sun.net.httpserver */
            try
              {
                gnu.kawa.servlet.KawaHttpHandler.addAutoHandler(args[iArg-1], args[iArg]);
              }
            catch (java.io.IOException ex)
              {
                throw new RuntimeException(ex);
              }
            catch (NoClassDefFoundError ex)
              {
                System.err.println("kawa: HttpServer classes not found");
                System.exit(-1);
              }
            /* #else */
            // System.err.println("kawa: HttpServer classes not found");
            // System.exit(-1);
            /* #endif */
          }
        else if (arg.equals("--http-start"))
          {
            iArg += 1;
	    if (iArg >= maxArg)
	      bad_option("missing httpd port argument");
            /* #ifdef use:com.sun.net.httpserver */
            int port;
            try
              {
                port = Integer.parseInt(args[iArg]);
              }
            catch (NumberFormatException ex)
              {
                bad_option("malformed server port#");
                port = -1; // never seen.
              }
            try
              {
                gnu.kawa.servlet.KawaHttpHandler.startServer(port);
              }
            catch (NoClassDefFoundError ex)
              {
                System.err.println("kawa: HttpServer classes not found");
                System.exit(-1);
              }
            catch (IOException ex)
              {
                throw new RuntimeException(ex);
              }
	    something_done = true;
            /* #else */
            // System.err.println("kawa: HttpServer classes not found");
            // System.exit(-1);
            /* #endif */
          }
	else if (arg.equals("--applet"))
	  {
	    defaultParseOptions |= Language.PARSE_FOR_APPLET;
	  }
	else if (arg.equals("--servlet"))
	  {
	    defaultParseOptions |= Language.PARSE_FOR_SERVLET;
            HttpRequestContext.importServletDefinitions = 2;
	  }
	else if (arg.equals("--debug-dump-zip"))
	  {
	    gnu.expr.ModuleExp.dumpZipPrefix = "kawa-zip-dump-";
	  }
	else if (arg.equals("--debug-print-expr"))
	  {
            Compilation.debugPrintExpr = true;
	  }
	else if (arg.equals("--debug-print-final-expr"))
	  {
	    Compilation.debugPrintFinalExpr = true;
	  }
	else if (arg.equals("--debug-syntax-pattern-match"))
	  {
	    SyntaxPattern.printSyntaxPatternMatch = true;
	  }
	else if (arg.equals("--debug-error-prints-stack-trace"))
	  {
            SourceMessages.debugStackTraceOnError = true;
	  }
	else if (arg.equals("--debug-warning-prints-stack-trace"))
	  {
            SourceMessages.debugStackTraceOnWarning = true;
	  }
        else if (arg.equals("--diagnostic-strip-directories"))
          SourceMessages.stripDirectoriesDefault = true;
	else if (arg.equals("--module-nonstatic")
                 || arg.equals("--no-module-static"))
	  {
	    Compilation.moduleStatic = Compilation.MODULE_NONSTATIC;
	  }
	else if (arg.equals("--module-static"))
	  {
	    Compilation.moduleStatic = Compilation.MODULE_STATIC;
	  }
        else if (arg.equals("--module-static-run"))
          {
            Compilation.moduleStatic = Compilation.MODULE_STATIC_RUN;
          }
	else if (arg.equals("--no-inline")
		 || arg.equals("--inline=none"))
	  {
	    Compilation.inlineOk = false;
	  }
        else if (arg.equals("--no-console"))
          InPort.noConsole = true;
	else if (arg.equals("--inline"))
	  {
	    Compilation.inlineOk = true;
	  }
	else if (arg.equals("--cps"))
	  {
	    Compilation.defaultCallConvention
	      = Compilation.CALL_WITH_CONTINUATIONS;
	  }
	else if (arg.equals("--full-tailcalls"))
	  {
	    Compilation.defaultCallConvention
	      = Compilation.CALL_WITH_TAILCALLS;
	  }
	else if (arg.equals("--no-full-tailcalls"))
	  {
	    Compilation.defaultCallConvention
	      = Compilation.CALL_WITH_RETURN;
	  }
        else if (arg.equals("--pedantic"))
          {
            Language.requirePedantic = true;
          }
	else if (arg.equals("--help"))
	  {
	    printOptions(System.out);
	    System.exit(0);
	  }
	else if (arg.equals("--author"))
	  {
	    System.out.println("Per Bothner <per@bothner.com>");
	    System.exit(0);
	  }
	else if (arg.equals("--version"))
	  {
	    System.out.print("Kawa ");
	    System.out.print(Version.getVersion());
	    System.out.println();
	    System.out.println("Copyright (C) 2014 Per Bothner");
	    something_done = true;
	  }
	else if (arg.length () > 0 && arg.charAt(0) == '-')
	  { // Check if arg is a known language name.
            boolean doubleDash = arg.length() > 2 && arg.charAt(1) == '-';
	    String name = arg.substring(doubleDash ? 2 : 1);
	    Language lang = Language.getInstance(name);
	    if (lang != null)
	      {
		if (previousLanguage == null)
		  Language.setDefaults(lang);
                else
                  Language.setCurrentLanguage(lang);
		previousLanguage = lang;
	      }
	    else
	      {
		// See if arg is a valid Compilation option, and if so set it.
		int eq = name.indexOf('=');
		String opt_value;
		if (eq < 0)
		  opt_value = null;
		else
		  {
		    opt_value = name.substring(eq+1);
		    name = name.substring(0, eq);
		  }

		// Convert "--no-xxx" to "--xxx=no":
		boolean startsWithNo
		  = name.startsWith("no-") && name.length() > 3;
		boolean addedNo = false;
                if (opt_value == null && startsWithNo)
		  {
		    opt_value = "no";
		    name = name.substring(3);
                    addedNo = true;
		  }

		String msg = Compilation.options.set(name, opt_value);
		if (msg != null)
		  {
		    if (msg == gnu.text.Options.UNKNOWN)
                      {
                        if (addedNo)
                          msg = "unknown option '"+name+"'";
                        // It wasn't a valid Compilation option.
                        else if (startsWithNo)
                          msg = "both '--no-' prefix and '="+
                            opt_value+"' specified";
                      }
		    if (msg == gnu.text.Options.UNKNOWN)
		      {
			bad_option(arg);
		      }
		    else
		      {
			System.err.println ("kawa: bad option '"
					    + arg + "': " + msg);
			System.exit (-1);
		      }
		  }
	      }
	  }
	else if (! ApplicationMainSupport.processSetProperty(arg))
          break;
      }
    // Adjust return value to index in *incoming* array.
    // This is a hack to compensate for meta-arg handling.
    return something_done ? -1 : iArg-returnDelta;
  }

  public static void compileFiles (String[] args, int iArg, int maxArg)
  {
    ModuleManager manager = ModuleManager.getInstance();
    Compilation[] comps = new Compilation[maxArg-iArg];
    ModuleInfo[] infos = new ModuleInfo[maxArg-iArg];
    SourceMessages messages = new SourceMessages();
    for (int i = iArg; i < maxArg;  i++)
      {
        String arg = args[i];
        getLanguageFromFilenameExtension(arg);
        Language language = Language.getDefaultLanguage();
        Compilation comp = null;
        try
          {
            InPort fstream;
            try
              {
                Path path = Path.valueOf(arg);
                fstream = Shell.openFile(path.openInputStream(), path);
              }
            catch (java.io.FileNotFoundException ex)
              {
                System.err.println(ex);
                System.exit(-1);
                break; // Kludge to shut up compiler.
              }
            
            ModuleInfo minfo = manager.findWithSourcePath(arg);
    
            if (compilationTopname != null)
              {
                String cname
                  = Compilation.mangleNameIfNeeded(compilationTopname);
                if (Compilation.classPrefixDefault != null)
                  cname = Compilation.classPrefixDefault + cname;
                minfo.setClassName(cname);
              }

            comp
              = language.parse(fstream, messages,
                               defaultParseOptions, minfo);
            infos[i-iArg] = minfo;
            comps[i-iArg] = comp;
          }
        catch (Exception ex)
          {
            if (! (ex instanceof SyntaxException)
                || ((SyntaxException) ex).getMessages() != messages)
              internalError(ex, comp, arg);
          }
        if (messages.seenErrorsOrWarnings())
          {
            System.err.println("(compiling "+arg+')');
            if (messages.checkErrors(System.err, 20))
              System.exit(1);
          }
      }

    for (int i = iArg; i < maxArg;  i++)
      {
        String arg = args[i];
        Compilation comp = comps[i-iArg];
        try
          {
            System.err.println("(compiling "+arg+" to "+comp.mainClass.getName()+')');

            infos[i-iArg].loadByStages(Compilation.CLASS_WRITTEN);
            boolean sawErrors = messages.seenErrors();
            messages.checkErrors(System.err, 50);
            if (sawErrors)
              System.exit(-1);
            comps[i-iArg] = comp; 
            sawErrors = messages.seenErrors();
            messages.checkErrors(System.err, 50);
            if (sawErrors)
              System.exit(-1);
          }
        catch (Exception ex)
          {
            internalError(ex, comp, arg);
          }
      }
  }

  static void internalError (Throwable ex, Compilation comp, Object arg)
  {
    try { comp.getMessages().checkErrors(System.err, 50); }
    catch (Exception e) { }
    StringBuffer sbuf = new StringBuffer();
    if (comp != null)
      {
        String file = comp.getFileName();
        int line = comp.getLineNumber();
        if (file != null && line > 0)
          {
            sbuf.append(file);
            sbuf.append(':');
            sbuf.append(line);
            sbuf.append(": ");
          }
      }
    sbuf.append("internal error while compiling ");
    sbuf.append(arg);
    System.err.println(sbuf.toString());
    ex.printStackTrace(System.err);
    System.exit(-1);
  }

  public static void main(String args[])
  {
    try
      {
        ExitCalled.push();
	int iArg = processArgs(args, 0, args.length);
	if (iArg < 0)
	  return;
        boolean ok;
	if (iArg < args.length)
	  {
	    String filename = args[iArg];
	    getLanguageFromFilenameExtension(filename);
	    setArgs (args, iArg+1);
	    checkInitFile();
	    ok = Shell.runFileOrClass(filename, false, 0);
	  }
	else
	  {
	    getLanguage();
	    setArgs (args, iArg);
	    checkInitFile();
            if (! InPort.haveConsole())
              startGuiConsole();
            else
              {
                ok = Shell.run(Language.getDefaultLanguage(),
                               Environment.getCurrent());
                if (! ok)
                  System.exit(-1);
              }
          }
      }
    finally
      {
	if (! shutdownRegistered)
	  {
	    // Redundant if registerShutdownHook succeeded (e.g on JDK 1.3).
	    OutPort.runCleanups();
	  }
	ModuleBody.exitDecrement();
        ExitCalled.pop();
      }
  }

  private static void startGuiConsole ()
  {
    // Do this instead of just new GuiConsole in case we have
    // configured --without-awt.
    try
      {
        Class.forName("kawa.GuiConsole").newInstance();
      }
    catch (Exception ex)
      {
        System.err.println("failed to create Kawa window: "+ex);
        System.exit (-1);
      }
  }
}
