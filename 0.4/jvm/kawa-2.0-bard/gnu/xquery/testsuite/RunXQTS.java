package gnu.xquery.testsuite;
import java.io.*;
import java.util.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.xml.*;
import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.FilePath;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import gnu.kawa.xml.*;
import gnu.mapping.Symbol;
import gnu.xquery.lang.*;
import gnu.text.SourceError;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import org.xml.sax.helpers.AttributesImpl;
import gnu.xquery.util.NodeUtils;

/** Run a suite of XQuery tests, as read from an xml file. */

public class RunXQTS extends FilterConsumer
{
  static XQuery xqueryLanguage = XQuery.getInstance();

  /* #ifdef JAVA5 */
  HashMap<String,String> expectedFailures = new HashMap<String,String>();
  HashMap<String,String> modules = new HashMap<String,String>();
  HashMap<String,Object> sources = new HashMap<String,Object>();
  Stack<Symbol> externalVariablesSet = new Stack<Symbol>();
  Stack<String> outputFileAlts = new Stack<String>();
  Stack<String> outputCompareAlts = new Stack<String>();
  /* #else */
  // Hashtable expectedFailures = new Hashtable();
  // Hashtable modules = new Hashtable();
  // Hashtable sources = new Hashtable();
  // Stack externalVariablesSet = new Stack();
  // Stack outputFileAlts = new Stack();
  // Stack outputCompareAlts = new Stack();
  /* #endif */
  ModuleManager manager = ModuleManager.getInstance();
  Object failExpected;

  boolean verbose = true;
  boolean useComments = true;

  String directory;
  String catalog;
  String XQTSVersion;
  String ResultOffsetPath;
  String XQueryQueryOffsetPath;
  String XQueryXQueryOffsetPath;
  String XQueryFileExtension;
  String XQueryXFileExtension;
  Object contextItem;

  int passCount;
  int xpassCount;
  int failCount;
  int xfailCount;
  int cannotTellCount;

  /** Set of expected error codes.  The format is "|Error1|..|ErrorN|". */
  StringBuffer expectedErrorsBuf = new StringBuffer("|");
  /** Same as expectedErrorBuf.toString() after collecting expected-errors. */
  String expectedErrors;

  String logFileName = "XQTS.log";
  XMLPrinter xqlog;

  String collectionID;
  Values.FromTreeList collectionDocuments;

  private void summaryReport (int count, String label)
  {
    if (count > 0)
      {
        System.out.print(label);
        System.out.println(count);
      }
  }

  private void summaryReport ()
  {
    summaryReport(passCount, "# of expected passes      ");
    summaryReport(xfailCount, "# of expected failures    ");
    summaryReport(xpassCount, "# of unexpected successes ");
    summaryReport(failCount, "# of unexpected failures  ");
    summaryReport(cannotTellCount, "# of cannot-tell (Inspect) results  ");
  }

  public static final String XQTS_RESULT_NAMESPACE
  = "http://www.w3.org/2005/02/query-test-XQTSResult";

  static Object testSuiteResultElementType;
  static
  {
    NamespaceBinding namespaceNodes
      = new NamespaceBinding(null, XQTS_RESULT_NAMESPACE,
            new NamespaceBinding("q",
                                 XQuery.QEXO_FUNCTION_NAMESPACE,
                                 NamespaceBinding.predefinedXML));
    Symbol sym = Symbol.make(XQTS_RESULT_NAMESPACE, "test-suite-result", "");
    testSuiteResultElementType = new XName(sym, namespaceNodes);
  }
  static Object testRunElementType
    = Symbol.make(XQTS_RESULT_NAMESPACE, "test-run", "");
  static Object testSuiteElementType
    = Symbol.make(XQTS_RESULT_NAMESPACE, "test-suite", "");
  static Object testCaseElementType
    = Symbol.make(XQTS_RESULT_NAMESPACE, "test-case", "");

  private void writeStartElement (String name)
  {
    xqlog.startElement(Symbol.make(XQTS_RESULT_NAMESPACE, name, ""));
  }

  private void writeStartAttribute (String name)
  {
    xqlog.startAttribute(name);
  }

  private void writeAttribute (String name, String value)
  {
    writeStartAttribute(name);
    xqlog.write(value);
    xqlog.endAttribute();
  }

  private void writeQexoAttribute (String name, String value)
  {
    xqlog.startAttribute(Symbol.make(XQuery.QEXO_FUNCTION_NAMESPACE,
                                     name, "q"));
    xqlog.write(value);
    xqlog.endAttribute();
  }

  private void writeVerbose (String name, String value)
  {
    if (useComments)
      {
        // The tricky part is to make sure that the result can be validated.
        // Specifically, no spaces are allowed in a <test-case>.
        xqlog.printIndent = -1;
        xqlog.beginComment();
        xqlog.printIndent = 0;
        xqlog.writeBreakFill();
        xqlog.write(name); xqlog.write(": ");
        xqlog.write(value);
        xqlog.writeBreakFill();
        xqlog.endComment();
      }
    else
      writeQexoAttribute(name, value);
  }

  public static void main (String[] args)
  {
    gnu.xquery.lang.XQuery.registerEnvironment();
    Language.requirePedantic = true;
    for (int i = 0;  i < args.length;  i++)
      {
	try
	  {
            RunXQTS runner = new RunXQTS(new CharArrayOutPort());
            runner.directory = args[i];
            runner.catalog = runner.directory + "/XQTSCatalog.xml";
            System.err.println("catalog: "+runner.catalog);
            XMLPrinter xqlog
              = new XMLPrinter(new BufferedOutputStream(new FileOutputStream(runner.logFileName)),
                               FilePath.valueOf(runner.logFileName));
            runner.xqlog = xqlog;
            xqlog.setPrintXMLdecl(true);
            xqlog.setStyle("xml");
            xqlog.useEmptyElementTag = 1;
            Object saveIndent = XMLPrinter.indentLoc.setWithSave("pretty");
            xqlog.startDocument();
            XMLPrinter.indentLoc.setRestore(saveIndent);

	    Document.parse(runner.catalog, runner);
            xqlog.endDocument();
            runner.summaryReport();
            xqlog.close();
	  }
        catch (OutOfMemoryError ex)
          {
            throw ex;
          }
	catch (Throwable ex)
	  {
            ex.printStackTrace();
	    System.err.println("caught "+ex+" while processing "+args[i]);
	  }
      }
  }

  int nesting = 0;
  Object currentElementType;
  Symbol currentElementSymbol;
  /* #ifdef JAVA5 */
  Stack<Object> elementTypeStack = new Stack<Object>();
  /* #else */
  // Stack elementTypeStack = new Stack();
  /* #endif */
  boolean inStartTag;
  int attrValueStart;
  // Start in cout's buffer of current element, indexed by nesting level.
  int[] elementStartIndex = new int[20];
  AttributesImpl attributes = new AttributesImpl();

  String query = null;
  String expect = null;

  CharArrayOutPort cout;

  public RunXQTS(CharArrayOutPort out)
  {
    super(out);
    this.cout = out;

    expectFailures("K2-MatchesFunc-1", "allow bad regex pattern");
    expectFailures("K2-MatchesFunc-8|K2-MatchesFunc-9|K2-MatchesFunc-10|K2-ReplaceFunc-3", "backwards references in regep not implemented");
    expectFailures("ST-WhereExpr001",
                   "testsuite error (4024) - should not require error");
    /* #ifndef JAVA5 */
    // expectFailures("surrogates12|surrogates13|surrogates14|surrogates15",
    //                "surrogates not handled by java.util.regex");
    /* #endif */
    expectFailures("K2-Literals-34", "MainModule without QueryBody"); // should be fixed
    expectFailures("K2-NameTest-68", "depends on typing");
    expectFailures("ST-Axes001|ST-Axes002|ST-Axes003|ST-Axes004|ST-Axes005|"
                   +"ST-Axes006|ST-Axes007|ST-Axes008|ST-Axes009|ST-Axes010|"
                   +"ST-Axes011|ST-Axes012|ST-Axes013|ST-Axes014|ST-Axes015",
                   "depends on static typing feature");
    expectFailures("fn-id-dtd-5|fn-id-dtd-7|fn-id-dtd-8|fn-id-dtd-9|"
                   +"fn-id-dtd-12|fn-id-dtd-13|fn-id-dtd-15|fn-id-dtd-16|"
                   +"fn-id-dtd-17|fn-id-dtd-18|fn-id-dtd-19|"
                   +"fn-id-dtd-20|fn-id-dtd-21|fn-id-dtd-23|",
                   "fn:id only works with xml:id so far");
    expectFailures("fn-idref-dtd-5|fn-idref-dtd-7|fn-idref-dtd-8|"
                   +"fn-idref-dtd-9|fn-idref-dtd-12|fn-idref-dtd-13|"
                   +"fn-idref-dtd-14|fn-idref-dtd-15|fn-idref-dtd-16|"
                   +"fn-idref-dtd-17|fn-idref-dtd-18|fn-idref-dtd-19|"
                   +"fn-idref-dtd-20|fn-idref-dtd-21|fn-idref-dtd-23|",
                   "fn:idref doesn't do much yet");
    /* #ifndef use:java.text.Normalizer */
    // expectFailures("fn-normalize-unicode1args-1|"
    //                +"fn-normalize-unicode1args-2|"
    //                +"fn-normalize-unicode1args-3|"
    //                +"fn-normalize-unicode1args-4|"
    //                +"fn-normalize-unicode1args-5|"
    //                +"fn-normalize-unicode1args-6|"
    //                +"fn-normalize-unicode2args-1|"
    //                +"fn-normalize-unicode2args-2|"
    //                +"fn-normalize-unicode2args-3|"
    //                +"fn-normalize-unicode-1|"
    //                +"fn-normalize-unicode-3|fn-normalize-unicode-4|"
    //                +"fn-normalize-unicode-5|fn-normalize-unicode-6|"
    //                +"fn-normalize-unicode-7|K-NormalizeUnicodeFunc-4|"
    //                +"K-NormalizeUnicodeFunc-5|K-NormalizeUnicodeFunc-6|"
    //                +"K-NormalizeUnicodeFunc-7|K-NormalizeUnicodeFunc-8|"
    //                +"K-NormalizeUnicodeFunc-11|K-NormalizeUnicodeFunc-12",
    //                "fn:normalize-unicode not unimplemented yet");
    /* #endif */
    // RunXQTS failures rather than Qexo errors:
    // Some work under gcj but not JDK 1.4.x or 1.5.0_05:
    expectFailures("K-TimeAddDTD-1|K-TimeAddDTD-2|K-TimeSubtractDTD-1",
                   "bad interaction between fields and millis");
    expectFailures("op-time-greater-than-2",
                   "comparing xs:time doesn't handle differing timezones");
    expectFailures("K-SubstringBeforeFunc-5|K-SubstringAfterFunc-5|"
                   +"K-ContainsFunc-5|K-StartsWithFunc-5|K-EndsWithFunc-5",
                   "some string functions don't support collation argument");
    expectFailures("caselessmatch04",
                   "regex/unicode special case");
    expectFailures("string-queries-results-q4|K2-FunctionProlog-7|K2-FunctionProlog-17|K2-FunctionProlog-18|K2-FunctionProlog-19|K2-FunctionProlog-22|XMark-Q18|XMark-All",
                   "function conversion incorrect for user-defined functions");
    expectFailures("caselessmatch10|caselessmatch11",
                   // Need to translate [xxx-[yyy]] to [xxx&&[^yyy]].
                   "regex range subtraction not implemented");
  }

  private void expectFailures (String testNames, String reason)
  {
    while (testNames != null)
      {
        int dot = testNames.indexOf('|');
        String testName;
        if (dot >= 0)
          {
            testName = testNames.substring(0, dot);
            testNames = testNames.substring(dot+1);
          }
        else
          {
            testName = testNames;
            testNames = null;
          }
        if (testName.length() > 0)
          expectedFailures.put(testName, reason);
      }
  }

  public void startElement(Object type)
  {
    if (inStartTag)
      handleStartTag();
    attributes.clear();
    inStartTag = true;
    elementTypeStack.push(currentElementType);
    currentElementType = type;
    currentElementSymbol = type instanceof Symbol ? (Symbol) type : null;
    /*
    System.err.println("startElement "+typeName);
    if ("test-suite".equals(typeName) && nesting == 0)
      inTestSuite = true;
    else if ("test-element".equals(typeName))
      {
      }
    else if ("test".equals(typeName)
	&& (nesting == 0 || (inTestSuite && nesting == 1)))
      inTest = true;
    else if (inTestSuite ? nesting == 2 : nesting == 1)
      {
	cout.setLength(0);
	currentTag = typeName;
      }
    else if (currentTag == null)
      throw new RuntimeException("saw <"+typeName+"> not in <test>");
    else
      base.startElement(type);
    */
    nesting++;
  }

  boolean tagMatches (String localName)
  {
    if (localName.equals(currentElementSymbol.getLocalName()))
      // also check uri FIXME
      return true;
    return false;
  }

  public void handleStartTag ()
  {
    elementStartIndex[nesting] = cout.length();
    if (tagMatches("test-suite"))
      {
        XQueryQueryOffsetPath = attributes.getValue("XQueryQueryOffsetPath");
        XQueryXQueryOffsetPath = attributes.getValue("XQueryXQueryOffsetPath");
        XQueryFileExtension = attributes.getValue("XQueryFileExtension");
        XQueryXFileExtension = attributes.getValue("XQueryXFileExtension");
        ResultOffsetPath = attributes.getValue("ResultOffsetPath");
        XQTSVersion = attributes.getValue("version");
 
        xqlog.startElement(testSuiteResultElementType);
        writeStartElement("implementation");
        writeAttribute("name", "Qexo");
        writeAttribute("version", kawa.Version.getVersion());
        writeStartElement("organization");
        writeAttribute("name", "GNU / Per Bothner");
        xqlog.endElement();
        writeStartElement("submittor");
        String user = System.getProperty("user.name");
        if ("bothner".equals(user))
          {
            writeAttribute("name", "Per Bothner");
            writeAttribute("email", "per@bothner.com");
          }
        else
          writeAttribute("name", user);
        xqlog.endElement();
        xqlog.endElement();
        writeStartElement("syntax");
        xqlog.write("XQuery");
        xqlog.endElement();
        xqlog.startElement(testRunElementType);
        StringBuffer sbuf = new StringBuffer();
        gnu.kawa.xml.XTimeType.dateTimeType.now().toStringDate(sbuf);
        writeAttribute("dateRun", sbuf.toString());
        xqlog.startElement(testSuiteElementType);
        writeAttribute("version", XQTSVersion);
        xqlog.endElement();
        xqlog.endElement();

      }
    else if (tagMatches("test-element"))
      {
        xqlog.writeComment("test-element "+attributes.getValue("name"));
      }
    else if (tagMatches("test-case"))
      {
        testName = attributes.getValue("name");
        scenario = attributes.getValue("scenario");
        testFilePath = attributes.getValue("FilePath");
        testQueryName = null;
        outputFileAlts.clear();
        outputCompareAlts.clear();
        expectedErrorsBuf.setLength(1);
        manager.clear();
        ModuleContext.getContext().clear();
        Document.clearLocalCache();
      }
    else if (tagMatches("query"))
      {
        testQueryName = attributes.getValue("name");
      }
    else if (tagMatches("source"))
      {
        String ID = attributes.getValue("ID");
        String filename = attributes.getValue("FileName");
        sources.put(ID, filename);
      }
    else if (testName == null && tagMatches("module"))
      {
        String ID = attributes.getValue("ID");
        String filename = attributes.getValue("FileName");
        modules.put(ID, filename);
      }
    else if (tagMatches("collection"))
      {
        collectionID = attributes.getValue("ID");
        collectionDocuments = new Values.FromTreeList();
        sources.put(collectionID, collectionDocuments);
      }
    inStartTag = false;
  }

  String testName;
  String scenario;
  String testQueryName;
  String testFilePath;
  String testQuery;

  int maxTests = -1;

  void report (String result, String comment)
  {
    boolean failed = "fail".equals(result);
    if (failExpected == null)
      {
        if (failed)
          {
            System.out.println("FAIL: "+testName);
            failCount++;
          }
        else if ("cannot tell".equals(result))
          cannotTellCount++;
        else
          passCount++;
      }
    else
      {
        if (failed)
          xfailCount++;
        else
          {
            System.out.println("XPASS: "+testName);
            xpassCount++;
          }
      }

    writeAttribute("result", result);

    if (failed && failExpected != null)
      {
        StringBuffer sbuf = new StringBuffer("(expected-to-fail: ");
        sbuf.append(failExpected.toString());
        sbuf.append(')');
        if (comment != null)
          {
            sbuf.append("; ");
            sbuf.append(comment);
          }
        comment = sbuf.toString();
      }
    if (comment != null)
      writeAttribute("comment", comment);
  }

  public void evalTest (String testName)
    throws Throwable
  {
    failExpected = expectedFailures.get(testName);
    if (failExpected == null)
      {
        // Check for a wildcard: replace a final non-negative integer by '*'.
        int len = testName.length();
        while (--len > 0 && Character.digit(testName.charAt(len), 10) >= 0);
        failExpected = expectedFailures.get(testName.substring(0, len+1)+'*');
      }
    Environment env = Environment.getCurrent();
    SourceMessages messages = new SourceMessages();
    String filename
      = directory + '/' + XQueryQueryOffsetPath + testFilePath
      + testQueryName + XQueryFileExtension;
    InPort in;
    expectedErrors = expectedErrorsBuf.toString();
    try
      {
        in = InPort.openFile(filename);
      }
    catch (java.io.FileNotFoundException ex)
      {
        String xfilename = directory + '/' + XQueryXQueryOffsetPath 
          + testFilePath + testQueryName + XQueryXFileExtension;
        if (new java.io.File(xfilename).exists())
          {
            report("fail", "xqueryx not implemented");
            return;
          }
        throw ex;
      }
    // This is mainly to avoid memory leaking.
    NameLookup.setInstance(xqueryLanguage.getEnvironment(), null);
    Compilation comp;
    Procedure withContextProc = null;
    try
      {
        if (contextItem != null)
          {
            withContextProc = xqueryLanguage.evalToFocusProc(in, messages);
            comp = null;
          }
        else
          comp = xqueryLanguage.parse(in, messages, Language.PARSE_IMMEDIATE);
        if (messages.seenErrors())
          throw new SyntaxException(messages);
      }
    catch (SyntaxException ex)
      {
        in.close();
        SourceError error = messages.getErrors();
        String errorString = error == null ? "" : "|" + error.code + "|";
        if (expectedErrors.indexOf(errorString) >= 0)
          {
            report("pass", null);
          }
        else if (errorString.equals("|XQST0009|"))
          {

            if (failExpected == null)
              failExpected = "'import schema' not implemented";
            report("fail", null);
          }
        else if (error.message != null
                 && error.message.indexOf("unknown type xs:NOTATION") >= 0
                 && (expectedErrors.equals("|XPST0080|")
                     || expectedErrors.equals("|XPST0017|")))
          {
            report("fail", null);
          }
        else if (error.message != null
                 && error.message.indexOf("unknown type xs:ENTITY") >= 0
                 && (expectedErrors.equals("|XPTY0004|")))
          {
            report("fail", null);
          }
        else if (error.message != null
                 && error.message.indexOf("unknown function") >= 0
                 && (expectedErrors.equals("|XPDY0002|")
                     || expectedErrors.equals("|XPTY0004|")
                     || expectedErrors.equals("|XQDY0025|")
                     || expectedErrors.equals("|FODC0001|")
                     || (expectedErrors.equals("|XPST0017|")
                         && (error.message.endsWith(" fn:id")
                             || error.message.endsWith(" fn:idref")))))

          {
            report("fail", null);
          }
        else if (expectedErrors.length() > 1)
          report("pass", "static error: "+error+" expected:"+expectedErrors);
        else
          report("fail", "static error: "+error.message);
        return;
      }
    in.close();

    CallContext ctx = CallContext.getInstance();
    if (contextItem != null)
      {
	gnu.math.IntNum one = gnu.math.IntNum.one();
        withContextProc.check3(contextItem, one, one, ctx);
      }
    gnu.lists.Consumer save = ctx.consumer;
    CharArrayOutPort out = new CharArrayOutPort();
    XMLPrinter xout = new XMLPrinter(out, false);
    xout.strict = true;
    xout.useEmptyElementTag = 1;
    xout.escapeNonAscii = false;
    xout.canonicalizeCDATA = true;
    ctx.consumer = xout;
    try
      {
        if (contextItem != null)
          ctx.runUntilDone();
        else
          ModuleExp.evalModule(env, ctx, comp, null, null);
      }
    catch (OutOfMemoryError ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
        if (ex instanceof NumberFormatException
            && expectedErrors.indexOf("|FORG0001|") >= 0)
          report("pass", "caught NumberFormatException expected:"+expectedErrors);
        else if (ex instanceof ClassCastException
                 && (expectedErrors.indexOf("|XPTY0004|") >= 0
                     || expectedErrors.indexOf("|XPTY0020|") >= 0
                     || expectedErrors.indexOf("|FORG0001|") >= 0
                     || expectedErrors.indexOf("|FOAR0002|") >= 0))
          report("pass", "caught ClassCastException expected:"+expectedErrors);
        else if (expectedErrors.length() > 1)
          report("pass", "caught "+ex+" expected:"+expectedErrors);
        else
          {
            report("fail", "caught "+ex);
            if (verbose)
              {
                CharArrayWriter wr = new CharArrayWriter();
                PrintWriter pr = new PrintWriter(wr);
                ex.printStackTrace(pr);
                pr.flush();
                writeVerbose("stack", wr.toString());
                wr.close();
              }
          }
        return;
      }

    if (messages.seenErrors())
      {
        if (expectedErrors.length() > 1)
          report("pass", "error: "+messages.getErrors()+" expected: "+expectedErrors);
        else
          report("fail", "error: "+messages.getErrors());
        return;
      }

    if ("trivial".equals(scenario))
      {
        failExpected = "trivial embedding not implemented";
        report("fail", null);
        return;
      }

    String actual = new String(out.toCharArray());
    byte[] expectedBytes = new byte[1024];
    xout.close();
    ctx.consumer = save;
    
    int numOutputFileAlts = outputFileAlts.size();
    boolean foundMatchingOutput = false;
    boolean displayDifference = false;
    String expected = null;
    String compare = null;
    for (int ialt = 0;  ialt < numOutputFileAlts;  ialt++)
      {
        compare = (String) outputCompareAlts.elementAt(ialt);
        if ("Ignore".equals(compare))
          {
            report("pass", null);
            foundMatchingOutput = true;
            break;
          }
        String outname  = directory + '/' + ResultOffsetPath + testFilePath
          + outputFileAlts.elementAt(ialt);
        FileInputStream expectStream = new FileInputStream(outname);
        int expectedLength = 0;
        for (;;)
          {
            int avail = expectedBytes.length-expectedLength;
            if (avail < 1024)
              {
                byte[] tmp = new byte[2*expectedBytes.length];
                System.arraycopy(expectedBytes, 0, tmp, 0, expectedLength);
                expectedBytes = tmp;
              }
            int n = expectStream.read(expectedBytes, expectedLength, avail);
            if (n < 0)
              break;
            expectedLength += n;
          }
        expectStream.close();
        expected = new String(expectedBytes, 0, expectedLength, "UTF-8");
        expected = expected.replaceAll("\r", "");
        actual = actual.replaceAll("\r", "");
        boolean matches = matches(actual, expected, compare);
        if (matches)
          {
            report("pass", null);
            foundMatchingOutput = true;
            break;
          }
        else if ("Inspect".equals(compare))
          {
            report("cannot tell", null);
            foundMatchingOutput = true;
            displayDifference = verbose;
            break;
          }
      }

    if (! foundMatchingOutput)
      {
        if (expectedErrors.length() > 1)
          {
            report("fail", "expected error: "+expectedErrors);
            return;
          }
        else
          {
            report("fail", null);
            if (verbose && expectedFailures.get(testName) == null)
              displayDifference = true;
          }
      }
    if (displayDifference)
      { // This only displays a single expected result.
        writeVerbose("compare", compare);
        writeVerbose("expected", expected);
        writeVerbose("actual", actual);
      }
  }

  private static int grabAttribute (String str, int start)
  {
    char inAttr = 0;
    for (int i = start; ; )
      {
        if (i >= str.length())
          return -1;
        char ch = str.charAt(i++);
        if (inAttr == 0 && (ch == '\"' || ch == '\''))
          inAttr = ch;
        else if (ch == inAttr)
          return i;
      }
  }

  public static boolean matches (String arg1 /* result */,
                                 String arg2 /*expected*/,
                                 String compare)
  {
    int len1 = arg1.length();
    int len2 = arg2.length();
    int i1 = 0, i2 = 0;
    boolean intag = false;
    int start_attr1 = 0;
    int start_attr2 = 0;
    boolean isXML = "XML".equals(compare) || "Fragment".equals(compare);
    char inAttr = 0;
    for (;;)
      {
        if (i1 == len1 && i2 == len2)
          return true;
        int c1 = i1 == len1 ? -1 : arg1.charAt(i1);
        int c2 = i2 == len2 ? -1 : arg2.charAt(i2);
        if ((c1 == '&' && arg1.charAt(i1+1) == '#' && i1 + 3 < len1)
            || (c2 == '&' && arg2.charAt(i2+1) == '#' && i2 + 3 < len2))
          {
            try
              {
                if (c1 == '&')
                  {
                    int base1 = 10;
                    i1 += 2;
                    if (arg1.charAt(i1) == 'x')
                      {
                        i1++;
                        base1 = 16;
                      }
                    int semi1 = arg1.indexOf(';', i1);
                    c1 = Integer.parseInt(arg1.substring(i1, semi1), base1);
                    i1 = semi1;
                  }
                else if (c1 >= 0xD800 && c1 < 0xDC00 && i1 + 1 < len1)
                  c1 = (c1 - 0xD800) * 0x400
                    + (arg1.charAt(++i1) - 0xDC00) + 0x10000;
                if (c2 == '&')
                  {
                    int base2 = 10;
                    i2 += 2;
                    if (arg2.charAt(i2) == 'x')
                      {
                        i2++;
                        base2 = 16;
                      }
                    int semi2 = arg2.indexOf(';', i2);
                    c2 = Integer.parseInt(arg2.substring(i2, semi2), base2);
                    i2 = semi2;
                  }
                else if (c2 >= 0xD800 && c2 < 0xDC00 && i2 + 1 < len2)
                  c2 = (c2 - 0xD800) * 0x400
                    + (arg2.charAt(++i2) - 0xDC00) + 0x10000;
              }
            catch (OutOfMemoryError ex)
              {
                throw ex;
              }
            catch (Throwable ex)
              {
                return false;
              }
            if (c1 != c2)
              return false;
            i1++;
            i2++;
          }
        else if (c1 == c2)
          {
            if (c1 == '<' && isXML)
              {
                intag = true;
                start_attr1 = 0;
                start_attr2 = 0;
                inAttr = 0;
              }
            else if (intag && c1 == '>')
              {
                intag = false;
              }
            else if (intag && Character.isWhitespace((char) c1)
                     && inAttr == 0)

              {
                start_attr1 = i1+1;
                start_attr2 = i2+1;;
              }
            else if (intag && inAttr == 0 && (c1 == '"' || c1 == '\''))
              {
                inAttr = (char) c1;
              }
            else if (intag && inAttr == c1)
              {
                start_attr1 = 0;
                start_attr2 = 0;
                inAttr = 0;
              }
            i1++;
            i2++;
          }
        else if (intag && start_attr1 > 0)
          {
            i1 = start_attr1;
            i2 = start_attr2;
            /* #ifdef JAVA5 */
            Stack<String> attrs1 = new Stack<String>();
            Stack<String> attrs2 = new Stack<String>();
            /* #else */
            // Stack attrs1 = new Stack();
            // Stack attrs2 = new Stack();
            /* #endif */
            for (;;)
              {
                int end1 = grabAttribute(arg1, i1);
                int end2 = grabAttribute(arg2, i2);
                if (end1 < 0 || end2 < 0)
                  return false;
                String attr1 = arg1.substring(i1, end1);
                attrs1.push(attr1);
                String attr2 = arg2.substring(i2, end2);
                attrs2.push(attr2);
                i1 = end1;
                i2 = end2;
                for (;;)
                  {
                    if (i1 >= len1) return false;
                    c1 = arg1.charAt(i1++);
                    if (! Character.isWhitespace((char) c1))
                      break;                }
                for (;;)
                  {
                    if (i2 >= len2) return false;
                    c2 = arg2.charAt(i2++);
                    if (! Character.isWhitespace((char) c2))
                      break;
                  }
                boolean done1 = c1 == '/' || c1 == '>';
                boolean done2 = c2 == '/' || c2 == '>';
                if (done1 && done2)
                  break;
                if (done1 || done2)
                  return false;
                i1--;
                i2--;
              }
            // Same number of attributes.
            // Do an O(n^2) search to make sure the sets are equal.
            for (int i = attrs1.size();  --i >= 0; )
              {
                String attr1 = (String) attrs1.elementAt(i);
                for (int j = attrs2.size();  ; )
                  {
                    if (--j < 0)
                      return false;
                    String attr2 = (String) attrs2.elementAt(j);
                    if (attr1.equals(attr2))
                      break;
                  }
              }
            start_attr1 = 0;
            start_attr2 = 0;
            intag = false;
          }
        else if (isFloatChar(c1)
                 ? (isFloatChar(c2) || (i2 > 0 && isFloatChar(arg2.charAt(i2-1))))
                 : (isFloatChar(c2) && (i1 > 0 && isFloatChar(arg1.charAt(i1-1)))))
          {
            int start1 = i1, start2 = i2;
            while (start1 > 0 && isFloatChar(arg1.charAt(start1-1)))
              start1--;
            while (start2 > 0 && isFloatChar(arg2.charAt(start2-1)))
              start2--;
            int end1 = i1, end2 = i2;
            while (end1 < len1 && isFloatChar(arg1.charAt(end1)))
              end1++;
            while (end2 < len2 && isFloatChar(arg2.charAt(end2)))
              end2++;
            if (end1 <= start1 || end2 <= start2)
              return false;
            String word1 = arg1.substring(start1, end1);
            String word2 = arg2.substring(start2, end2);
            try
              {
                float f1 = Float.parseFloat(word1);
                float f2 = Float.parseFloat(word2);
                if (Float.floatToIntBits(f1) != Float.floatToIntBits(f2))
                  return false;
              }
            catch (OutOfMemoryError ex)
              {
                throw ex;
              }
            catch (Throwable ex)
              {
                return false;
              }
            i1 = end1;
            i2 = end2;
          }
        else if (isXML && (c1 == ' ' || c1 == '\n' || c1 == '\t' || c1 == '\r'))
          {
            i1++;
          }
        else if (isXML && (c2 == ' ' || c2 == '\n' || c2 == '\t' || c2 == '\r'))
          {
            i2++;
          }
        // If isXML, then "/>" matches "></ANYNAME>".
        else if (isXML && c1 == '/' && c2 == '>'
                 && i1 + 1 < len1 && i2 + 2 < len2
                 && arg1.charAt(i1+1) == '>'
                 && arg2.charAt(i2+1) == '<'
                 && arg2.charAt(i2+2) == '/')
          {
            for (i2 = i2 + 3; ; i2++)
              {
                if (i2 >= len2)
                  return false;
                char c = arg2.charAt(i2);
                if (c == '>')
                  break;
                if (! XName.isNamePart(c))
                  return false;
              }
            i1 = i1 + 2;
            i2 = i2 + 1;
          }
        else
          return false;
      }
  }

  static boolean isFloatChar (int c)
  {
    return (c >= '0' && c <= '9')
      || c == '.' || c == '-' || c == '+' || c == 'E';
  }

  public String getElementValue ()
  {
    return cout.toSubString(elementStartIndex[nesting]);
  }

  String selectedTest;

  boolean isSelected (String testName)
  {
    // it's too slow - slow implementation of indexing of large sequence.
    if ("Constr-cont-document-3".equals(testName))
      return false;
    // infinite recursion
    if ("K2-InternalVariablesWithout-9".equals(testName))
      return false;
    return selectedTest == null || selectedTest.equals(testName);
  }

  public void endElement()
  {
    if (inStartTag)
      handleStartTag();
    if (tagMatches("test-case"))
      {
        if (--maxTests == 0)  System.exit(0); // FIXME
        if (isSelected(testName))
          {
            xqlog.startElement(testCaseElementType);
            writeAttribute("name", testName);
            try
              {
                // Other attributes and <test-case> body written by evalTest.
                evalTest(testName);
              }
            catch (OutOfMemoryError ex)
              {
                throw ex;
              }
            catch (Throwable ex)
              {
                System.err.println("test-case name:"+testName);
                System.err.println("caught "+ex);
                ex.printStackTrace();
              }
            xqlog.endElement();
          }
        //xqlog.flush();
        testName = null;
        contextItem = null;
        Environment env = Environment.getCurrent();
        while (! externalVariablesSet.empty())
          env.remove((Symbol) externalVariablesSet.pop());
      }
    else if (tagMatches("expected-error"))
      {
        expectedErrorsBuf.append(getElementValue());
        expectedErrorsBuf.append('|');
      }
    else if (tagMatches("input-query"))
      {
        if (isSelected(testName))
          {
            String variable = attributes.getValue("variable");
            Symbol symbol = Symbol.parse(variable);
            String name = attributes.getValue("name");
            String filename
              = directory + '/' + XQueryQueryOffsetPath + testFilePath
              + name + XQueryFileExtension;
            InPort in;
            try
              {
                in = InPort.openFile(filename);
                Object value = XQuery.getInstance().eval(in);
                in.close();
                Environment current = Environment.getCurrent();
                current.define(symbol, null, value);
                externalVariablesSet.push(symbol);
              }
            catch (OutOfMemoryError ex)
              {
                throw ex;
              }
            catch (Throwable ex)
              {
                System.err.println("input-query for "+testName+": cannot open "+filename);
                System.err.println("caught "+ex);
                ex.printStackTrace();
                System.exit(-1);
              }
          }
      }
    else if (tagMatches("input-file") || tagMatches("contextItem"))
      {
        String inputFile = getElementValue();
        // KLUDGE around testsuite bug!
        if ("userdefined".equals(inputFile))
          inputFile = "emptydoc";
        String path = directory + '/' + sources.get(inputFile);

        String variable;
        Symbol symbol;
        if (tagMatches("input-file"))
          {
            variable = attributes.getValue("variable");
            symbol = Symbol.parse(variable);
            externalVariablesSet.push(symbol);
         }
        else // tagMatches("contextItem")
          {
            variable = null;
            symbol = null;
          }
        try
          {
            Object value = gnu.kawa.xml.Document.parseCached(path);
            if (symbol != null)
              Environment.getCurrent().define(symbol, null, value);
            else
              contextItem = value;
          }
        catch (OutOfMemoryError ex)
          {
            throw ex;
          }
        catch (Throwable ex)
          {
            System.err.println("caught "+ex);
            System.err.println("reading data file "+path+" for "+testName);
            System.err.println("inputFile:"+inputFile+" variable:"+variable+" path:"+path);
            ex.printStackTrace();
            System.exit(-1);
          }
      }
    else if (tagMatches("input-URI"))
      {
        String inputFile = getElementValue();
        String variable = attributes.getValue("variable");
        Object inputValue = sources.get(inputFile);
        String path = inputValue instanceof Values ? "collection:"+inputFile
          : "file://" + directory + '/' + inputValue;
        Symbol symbol = Symbol.parse(variable);
        externalVariablesSet.push(symbol);
        Environment.getCurrent().put(symbol, null, Path.valueOf(path));
      }
    else if (tagMatches("defaultCollection"))
      {
        String inputFile = getElementValue();
        Object val = NodeUtils.getSavedCollection("collection:"+inputFile);
        NodeUtils.setSavedCollection("#default", val);
      }
    else if (tagMatches("collection"))
      {
        NodeUtils.setSavedCollection("collection:"+collectionID, 
                                     collectionDocuments.canonicalize());
        collectionID = null;
        collectionDocuments = null;
      }
    else if (tagMatches("input-document"))
      {
        String inputName = getElementValue();
        String path = "file://" + directory + '/' + sources.get(inputName);
        if (collectionID == null)
          throw new Error("<input-document> not in <collection>");
        try
          {
            KDocument value = (KDocument) Document.parseCached(path);
            collectionDocuments.writeObject(value);
          }
        catch (OutOfMemoryError ex)
          {
            throw ex;
          }
        catch (Throwable ex)
          {
            System.err.println("caught "+ex);
            System.err.println("reading data file "+path);
            System.err.println("for collection "+collectionID);
            ex.printStackTrace();
            System.exit(-1);
          }
      }
    else if (tagMatches("output-file"))
      {
        outputFileAlts.push(getElementValue());
        outputCompareAlts.push(attributes.getValue("compare"));
      }
    else if (tagMatches("test-suite"))
      {
        xqlog.endElement();
      }
    else if (testName != null && tagMatches("module"))
      {
        String uri = attributes.getValue("namespace");
        String module = getElementValue();
        String mfile = (String) modules.get(module);
        String mpath = directory + '/' + mfile + XQueryFileExtension;
        String mclass = Compilation.mangleURI(uri)
          + '.' + XQuery.makeClassName(mpath);
        ModuleInfo minfo = manager.findWithSourcePath(mpath);
        minfo.setClassName(mclass);
        minfo.setNamespaceUri(uri);
      }
    /*
    else if ("test".equals(typeName)
	&& (nesting == 0 || (inTestSuite && nesting == 1)))
      {
	inTest = false;
	TestMisc.evalTest(query, expect);
      }
    else if (inTestSuite ? nesting == 2 : nesting == 1)
      {
	if ("query".equals(typeName))
	  query = sout.toString();
	else if ("expect".equals(typeName))
	  expect = sout.toString();
	currentTag = null;
      }
    else
      base.endElement();
    */
    cout.setLength(elementStartIndex[nesting]);
    nesting--;
    Object type = elementTypeStack.pop();
    currentElementType = type;
    currentElementSymbol = type instanceof Symbol ? (Symbol) type : null;
  }

  public void startAttribute(Object attrType)
  {
    super.startAttribute(attrType);
    attrValueStart = cout.length();
  }

  public void endAttribute()
  {
    super.endAttribute();
    String attrValue = cout.toSubString(attrValueStart, cout.length()-1);
    Symbol sym = (Symbol) attributeType;
    String uri = sym.getNamespaceURI();
    String local = sym.getLocalPart();
    String prefix = sym.getPrefix();
    String qname = (prefix == null || prefix.length() == 0 ? local
                    : prefix+":"+local);
    cout.setLength(attrValueStart);
    attributes.addAttribute(uri, local, qname, "CDATA", attrValue);
  }

  public void beforeContent ()
  {
    if (! inAttribute && inStartTag)
      handleStartTag();
  }
}
