// Copyright (c) 2001, 2003, 2005, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import java.io.*;
import gnu.math.RealNum;
import gnu.kawa.io.PrettyWriter;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.mapping.ThreadLocation;
import gnu.mapping.Symbol;
import gnu.text.Char;
import gnu.text.Lexer;
import java.math.BigDecimal;
import gnu.expr.Keyword;
import gnu.kawa.xml.XmlNamespace;

/** Print an event stream in XML format on a PrintWriter. */

public class XMLPrinter extends OutPort
  implements PositionConsumer, XConsumer
{
  /** Controls whether to add extra indentation.
   * -1: don't add indentation; 0: pretty-print (avoid needless newlines);
   * 1: indent (force). */
  public int printIndent = -1;
  /** When indenting, should attributes be lined up? */
  public boolean indentAttributes;

  boolean printXMLdecl = false;
  public void setPrintXMLdecl (boolean value) { printXMLdecl = value; }
  boolean inDocument;
  boolean inAttribute = false;
  boolean inStartTag = false;
  /** 0: not in comment; 1: in comment normal; 2: in comment after '-'. */
  int inComment;
  boolean needXMLdecl = false;
  boolean canonicalize = true;
  public boolean canonicalizeCDATA;
  /** Handling of empty elements.
   * 0: No element element tags, as required for canonical XML:
   * {@code <br></br>}.
   * 1: Use XML-style empty element tags: {@code <br/>}
   * 2: Use HTML-compatible empty element tags: {@code <br />}
   */
  public int useEmptyElementTag = 2;
  public boolean escapeText = true;
  public boolean escapeNonAscii = true;
  boolean isHtml = false;
  boolean isHtmlOrXhtml = false;
  boolean undeclareNamespaces = false;
  Object style;
  /** Fluid parameter to control whether a DOCTYPE declaration is emitted.
   * If non-null, this is the the public identifier. */
  public static final ThreadLocation doctypeSystem
    = new ThreadLocation("doctype-system");
  /** The system identifier emitted in a DOCTYPE declaration. 
   * Has no effect if doctypeSystem returns null.
   * If non-null, this is the the system identifier. */
  public static final ThreadLocation doctypePublic
    = new ThreadLocation("doctype-public");
  public static final ThreadLocation<String> indentLoc
    = new ThreadLocation<String>("xml-indent");

  public boolean strict;

  /** Chain of currently active namespace nodes. */
  NamespaceBinding namespaceBindings = NamespaceBinding.predefinedXML;

  /** Stack of namespaceBindings as of active startElement calls. */
  NamespaceBinding[] namespaceSaveStack = new NamespaceBinding[20];

  Object[] elementNameStack = new Object[20];

  /** Difference between number of startElement and endElement calls so far. */
  int elementNesting;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
  private static final int ELEMENT_START = -3;
  private static final int ELEMENT_END = -4;
  private static final int COMMENT = -5;
  private static final int KEYWORD = -6;
  private static final int PROC_INST = -7;
  int prev = ' ';

  char savedHighSurrogate; // should perhaps be combined with prev?

  public XMLPrinter (OutPort out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public XMLPrinter (Writer out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public XMLPrinter (OutputStream out, boolean autoFlush)
  {
    super(new OutputStreamWriter(out), true, autoFlush);
  }

  public XMLPrinter (Writer out)
  {
    super(out);
  }

  public XMLPrinter (OutputStream out)
  {
    super(new OutputStreamWriter(out), false, false);
  }

  public XMLPrinter (OutputStream out, Path path)
  {
    super(new OutputStreamWriter(out), true, false, path);
  }

  public static XMLPrinter make(OutPort out, Object style)
  {
    XMLPrinter xout = new XMLPrinter(out, true);
    xout.setStyle(style);
    return xout;
  }

  /** Convert argument to string in XML syntax. */

  public static String toString (Object value)
  {
    StringWriter stringWriter = new StringWriter();
    new XMLPrinter(stringWriter).writeObject(value);
    return stringWriter.toString();
  }

  public void setStyle (Object style)
  {
    this.style = style;
    useEmptyElementTag = canonicalize ? 0 : 1;
    if ("html".equals(style))
      {
	isHtml = true;
        isHtmlOrXhtml = true;
	useEmptyElementTag = 2;
        // Pre-establish the html namespace, so it doesn't get printed.
        if (namespaceBindings == NamespaceBinding.predefinedXML)
          namespaceBindings = XmlNamespace.HTML_BINDINGS;
      }
    else if (namespaceBindings == XmlNamespace.HTML_BINDINGS)
      namespaceBindings = NamespaceBinding.predefinedXML;
    if ("xhtml".equals(style))
      {
        isHtmlOrXhtml = true;
        useEmptyElementTag = 2;
      }
    if ("plain".equals(style))
      escapeText = false;
  }

  boolean mustHexEscape (int v)
  {
    return (v >= 127 && (v <= 159 || escapeNonAscii))
      || v == 0x2028
      // We must escape control characters in attributes,
      // since otherwise they get normalized to ' '.
      || (v < ' ' && (inAttribute || (v != '\t' && v != '\n')));
  }

  public void write (int v)
  {
    closeTag();
    if (printIndent >= 0)
      {
        if ((v == '\r' || v == '\n'))
          {
            if (v != '\n' || prev != '\r')
              writeBreak(PrettyWriter.NEWLINE_MANDATORY);
            if (inComment > 0)
              inComment = 1;
            return;
          }
      }
    if (! escapeText)
      {
	bout.write(v);
	prev = v;
      }
    else if (inComment > 0)
      {
        if (v == '-')
          {
            if (inComment == 1)
              inComment = 2;
            else
              bout.write(' ');
          }
        else
          inComment = 1;
        super.write(v);
      }
    else
      {
	prev = ';';
	if (v == '<' && ! (isHtml && inAttribute))
	  bout.write("&lt;");
	else if (v == '>')
	  bout.write("&gt;");
	else if (v == '&')
	  bout.write("&amp;");
	else if (v == '\"' && inAttribute)
	  bout.write("&quot;");
	else if (mustHexEscape(v))
          {
            int i = v;
            if (v >= 0xD800)
              {
                if (v < 0xDC00)
                  {
                    savedHighSurrogate = (char) v;
                    return;
                  }
                else if (v < 0xE000)
                  { // low surrogate
                    //if (highSurrogate < 0xDC00 || highSurrogate > 0xE000)
                    // error();
                    i = (savedHighSurrogate - 0xD800) * 0x400
                      + (i - 0xDC00) + 0x10000;
                    savedHighSurrogate = 0;
                  }
              }
            bout.write("&#x"+Integer.toHexString(i).toUpperCase()+";");
          }
	else
	  {
	    bout.write(v);
	    prev = v;
	  }
      }
  }

  private void startWord()
  {
    closeTag();
    writeWordStart();
  }

  public void writeBoolean(boolean v)
  {
    startWord();
    super.print(v);
    writeWordEnd();
  }

  protected void startNumber()
  {
    startWord();
  }

  protected void endNumber()
  {
    writeWordEnd();
  }

  public void closeTag()
  {
    if (inStartTag && ! inAttribute)
      {
	if (printIndent >= 0 && indentAttributes)
          endLogicalBlock("");
	bout.write('>');
	inStartTag = false;
	prev = ELEMENT_START;
      }
    else if (needXMLdecl)
      {
	// should also include encoding declaration FIXME.
	bout.write("<?xml version=\"1.0\"?>\n");
	if (printIndent >= 0)
	  {
	    startLogicalBlock("", "", 2);
	  }
	needXMLdecl = false;
        prev = '>';
      }
  }

  void setIndentMode ()
  {
    Object xmlIndent = indentLoc.get(null);
    String indent = xmlIndent == null ? null : xmlIndent.toString();
    if (indent == null)
      printIndent = -1;
    else if (indent.equals("pretty"))
      printIndent = 0;
    else if (indent.equals("always") || indent.equals("yes"))
      printIndent = 1;
    else // if (ident.equals("no")) or default:
      printIndent = -1;
  }

  public void startDocument()
  {
    if (printXMLdecl)
      {
	// We should emit an XML declaration, but don't emit it yet, in case
	// we get it later as a processing instruction.
	needXMLdecl = true;
      }
    setIndentMode();
    inDocument = true;
    if (printIndent >= 0 && ! needXMLdecl)
      startLogicalBlock("", "", 2);
  }

  public void endDocument()
  {
    inDocument = false;
    if (printIndent >= 0)
      endLogicalBlock("");
    freshLine();
  }

  public void beginEntity (Object base)
  {
  }

  public void endEntity ()
  {
  }

  protected void writeQName (Object name)
  {
    if (name instanceof Symbol)
      {
        Symbol sname = (Symbol) name;
        String prefix = sname.getPrefix();
        if (prefix != null && prefix.length() > 0)
          {
            bout.write(prefix);
            bout.write(':');
          }
        bout.write(sname.getLocalPart());
      }
    else
      bout.write(name == null ? "{null name}" : (String) name);
  }

  /** Write DOCTYPE using ThreadLocations doctypeSystem and doctypePublic */
  public void writeDoctypeIfDefined (String tagname)
  {
    Object systemIdentifier = doctypeSystem.get(null);
    if (systemIdentifier != null)
      {
        String systemId = systemIdentifier.toString();
        if (systemId.length() > 0)
          {
            Object publicIdentifier = doctypePublic.get(null);
            String publicId = publicIdentifier == null ? null
              : publicIdentifier.toString();
            writeDoctype(tagname, systemId, publicId);
          }
      }
  }

  public void writeDoctype (String tagname, String systemId, String publicId)
  {
    bout.write("<!DOCTYPE ");
    bout.write(tagname);
    if (publicId != null && publicId.length() > 0)
      {
        bout.write(" PUBLIC \"");
        bout.write(publicId);
        bout.write("\" \"");
      }
    else
      {
        bout.write(" SYSTEM \"");
      }
    bout.write(systemId);
    bout.write("\">");
    println();
  }

  public void startElement(Object type)
  {
    closeTag();
    if (elementNesting == 0)
      {
        if (! inDocument)
          setIndentMode();
        if (prev == PROC_INST)
          write('\n');
        if (type != null)
            writeDoctypeIfDefined(type.toString());
      }
    if (printIndent >= 0)
      {
	if (prev == ELEMENT_START || prev == ELEMENT_END || prev == COMMENT)
	  writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
			  : PrettyWriter.NEWLINE_LINEAR);
	startLogicalBlock("", "", 2);
      }
    bout.write('<');
    writeQName(type);
    if (printIndent >= 0 && indentAttributes)
      startLogicalBlock("", "", 2);
    elementNameStack[elementNesting] = type;
    NamespaceBinding elementBindings = null;
    namespaceSaveStack[elementNesting++] = namespaceBindings;
    if (type instanceof XName)
      {
	elementBindings = ((XName) type).namespaceNodes;
	NamespaceBinding join
	  = NamespaceBinding.commonAncestor(elementBindings, namespaceBindings);
        int numBindings = elementBindings == null ? 0
          : elementBindings.count(join);
        NamespaceBinding[] sortedBindings = new NamespaceBinding[numBindings];
        int i = 0;
        boolean sortNamespaces = canonicalize;
      check_namespaces:
	for (NamespaceBinding ns = elementBindings;  ns != join;  ns = ns.next)
          {
            int j = i;
            boolean skip = false;
            String uri = ns.getUri();
            String prefix = ns.getPrefix();
            while (--j >= 0)
              {
                NamespaceBinding ns_j = sortedBindings[j];
                // If (compare(ns, ns_j) <= 0) break:
                String prefix_j = ns_j.getPrefix();
                if (prefix == prefix_j)
                  continue check_namespaces;
                // If we're not canonicalizing, we just want to suppress
                // duplicates, rather than putting them in order.
                // Note we put the bindings in reverse order, since that's
                // what the following print loop expects.
                if (! sortNamespaces)
                  continue;
                if (prefix == null)
                  break;
                if (prefix_j != null && prefix.compareTo(prefix_j) <= 0)
                  break;
                sortedBindings[j+1] = ns_j;
              }
            if (sortNamespaces)
              j++;
            else
              j = i;
            sortedBindings[j] = ns;
            i++;
          }
        numBindings = i;
        // Note we print the bindings in reverse order, since the chain
        // is in reverse document order.
        for (i = numBindings; --i >= 0; )
	  {
            NamespaceBinding ns = sortedBindings[i];
	    String prefix = ns.prefix;
	    String uri = ns.uri;
	    if (uri == namespaceBindings.resolve(prefix))
	      // A matching namespace declaration is already in scope.
	      continue;
            if (uri == null && prefix != null && ! undeclareNamespaces)
              continue;
	    bout.write(' '); // prettyprint break
	    if (prefix == null)
	      bout.write("xmlns");
	    else
	      {
		bout.write("xmlns:");
		bout.write(prefix);
	      }
	    bout.write("=\"");
	    inAttribute = true;
	    if (uri != null)
	      write(uri);
	    inAttribute = false;
	    bout.write('\"');
	  }
	if (undeclareNamespaces)
	  {
	    // As needed emit namespace undeclarations as in
	    // the XML Namespaces 1.1 Candidate Recommendation.
	    // Most commonly this loop will run zero times.
	    for (NamespaceBinding ns = namespaceBindings;
		 ns != join;  ns = ns.next)
	      {
		String prefix = ns.prefix;
		if (ns.uri != null && elementBindings.resolve(prefix) == null)
		  {
		    bout.write(' '); // prettyprint break
		    if (prefix == null)
		      bout.write("xmlns");
		    else
		      {
			bout.write("xmlns:");
			bout.write(prefix);
		      }
		    bout.write("=\"\"");
		  }
	      }
	  }
	namespaceBindings = elementBindings;
      }
    if (elementNesting >= namespaceSaveStack.length)
      {
	NamespaceBinding[] nstmp = new NamespaceBinding[2 * elementNesting];
	System.arraycopy(namespaceSaveStack, 0, nstmp, 0, elementNesting);
	namespaceSaveStack = nstmp;
	Object[] nmtmp = new Object[2 * elementNesting];
	System.arraycopy(elementNameStack, 0, nmtmp, 0, elementNesting);
	elementNameStack = nmtmp;
      }

    inStartTag = true;
    
    String typeName = getHtmlTag(type);
    if ("script".equals(typeName) || "style".equals(typeName))
      escapeText = false;
  }

  static final String HtmlEmptyTags
  = "/area/base/basefont/br/col/frame/hr/img/input/isindex/link/meta/para/";

  public static boolean isHtmlEmptyElementTag(String name)
  {
    int index = HtmlEmptyTags.indexOf(name);
    return index > 0 && HtmlEmptyTags.charAt(index-1) == '/'
      && HtmlEmptyTags.charAt(index+name.length()) == '/';
  }

  protected String getHtmlTag (Object type)
  {
    if (type instanceof Symbol)
      {
        Symbol sym = (Symbol) type;
        String uri =  sym.getNamespaceURI();
        if (uri == XmlNamespace.XHTML_NAMESPACE
            || (isHtmlOrXhtml && uri == ""))
          return sym.getLocalPart();
      }
    else if (isHtmlOrXhtml)
      return type.toString();
    return null;
  }

  public void endElement ()
  {
    if (useEmptyElementTag == 0)
      closeTag();
    Object type = elementNameStack[elementNesting-1];

    // typeName is only used for checking certain HTML tags.
    String typeName = getHtmlTag(type);

    if (inStartTag)
      {
	if (printIndent >= 0 && indentAttributes)
	  {
	    endLogicalBlock("");
	  }
        String end = null;
        boolean isEmpty = typeName != null && isHtmlEmptyElementTag(typeName);
        if (useEmptyElementTag == 0
            || (typeName != null && ! isEmpty))
          {
            if (type instanceof Symbol)
              {
                Symbol sym = (Symbol) type;
                String prefix = sym.getPrefix();
                String uri = sym.getNamespaceURI();
                String local = sym.getLocalName();
                if (prefix != "")
                  end = "></"+prefix+":"+local+">";
                else if (uri == "" || uri == null)
                  end = "></"+local+">";
              }
          }
        if (end == null)
          end = isEmpty && isHtml ? ">" : useEmptyElementTag == 2 ? " />" : "/>";
        bout.write(end);
	inStartTag = false;
      }
    else
      {
	if (printIndent >= 0)
	  {
	    setIndentation(0, false);
	    if (prev == ELEMENT_END)
	      writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
                         : PrettyWriter.NEWLINE_LINEAR);
	  }
	bout.write("</");
        writeQName(type);
	bout.write(">");
      }
    if (printIndent >= 0)
      {
	endLogicalBlock("");
      }
    prev = ELEMENT_END;
    if (typeName != null && ! escapeText
	&& ("script".equals(typeName) || "style".equals(typeName)))
      escapeText = true;

    namespaceBindings = namespaceSaveStack[--elementNesting];
    namespaceSaveStack[elementNesting] = null;
    elementNameStack[elementNesting] = null;
  }

  /** Write a attribute for the current element.
   * This is only allowed immediately after a startElement. */
  public void startAttribute (Object attrType)
  {
    if (! inStartTag && strict)
      error("attribute not in element", "SENR0001");
    if (inAttribute)
      bout.write('"');
    inAttribute = true;
    bout.write(' ');
    if (printIndent >= 0)
      writeBreakFill();
    bout.write(attrType==null ? "{null name}" : attrType.toString());
    bout.write("=\"");
    prev = ' ';
  }

  public void endAttribute()
  {
    if (inAttribute)
      {
        if (prev != KEYWORD)
          {
            bout.write('"');
            inAttribute = false;
          }
        prev = ' ';
      }
  }

  public void writeDouble (double d)
  {
    startWord();
    bout.write(formatDouble(d));
  }

  public void writeFloat (float f)
  {
    startWord();
    bout.write(formatFloat(f));
  }

  /** Helper to format xs:double according to XPath/XQuery specification. */
  public static String formatDouble (double d)
  {
    if (Double.isNaN(d))
      return "NaN";
    boolean neg = d < 0;
    if (Double.isInfinite(d))
      return neg ? "-INF" : "INF";
    double dabs = neg ? -d : d;
    String dstr = Double.toString(d);
    // Unfortunately, XQuery's rules for when to use scientific notation
    // are different from Java's.  So fixup the string, if needed.
    if ((dabs >= 1000000 || dabs < 0.000001) && dabs != 0.0)
      return RealNum.toStringScientific(dstr);
    else
      return formatDecimal(RealNum.toStringDecimal(dstr));
  }

  /** Helper to format xs:float according to XPath/XQuery specification. */
  public static String formatFloat (float f)
  {
    if (Float.isNaN(f))
      return "NaN";
    boolean neg = f < 0;
    if (Float.isInfinite(f))
      return neg ? "-INF" : "INF";
    float fabs = neg ? -f : f;
    String fstr = Float.toString(f);
    // Unfortunately, XQuery's rules for when to use scientific notation
    // are different from Java's.  So fixup the string, if needed.
    if ((fabs >= 1000000 || fabs < 0.000001) && fabs != 0.0)
      return RealNum.toStringScientific(fstr);
    else
      return formatDecimal(RealNum.toStringDecimal(fstr));
  }

  /** Format java.math.BigDecimal as needed for XPath/XQuery's xs:decimal.
   * Specifically this means removing trailing fractional zeros, and a trailing
   * decimal point. However, note that the XML Schema canonical representation
   * does require a decimal point and at least one fractional digit.
   */
  public static String formatDecimal (BigDecimal dec)
  {
    /* #ifdef JAVA5 */
    return formatDecimal(dec.toPlainString());
    /* #else */
    // return formatDecimal(dec.toString());
    /* #endif */
  }

  static String formatDecimal (String str)
  {
    int dot = str.indexOf('.');
    if (dot >= 0)
      {
        int len = str.length();
        for (int pos = len; ; )
          {
            char ch = str.charAt(--pos);
            if (ch != '0')
              {
                if (ch != '.')
                  pos++;
                return pos == len ? str : str.substring(0, pos);
              }
          }
      }
    return str;
  }

  public void print(Object v)
  {
    if (v instanceof BigDecimal)
      v = formatDecimal((BigDecimal) v);
    else if (v instanceof Double || v instanceof gnu.math.DFloNum)
      v = formatDouble(((Number) v).doubleValue());
    else if (v instanceof Float)
      v = formatFloat(((Float) v).floatValue());
    write(v == null ? "(null)" : v.toString());
  }

  public void writeObject(Object v)
  {
    if (v instanceof SeqPosition)
      {
        bout.clearWordEnd();
	SeqPosition pos = (SeqPosition) v;
	pos.sequence.consumeNext(pos.ipos, this);
        if (pos.sequence instanceof NodeTree)
          prev = '-';
	return;
      }
    if (v instanceof Consumable)
      {
	((Consumable) v).consume(this);
	return;
      }
    if (v instanceof Keyword)
      {
        startAttribute(((Keyword) v).getName());
        prev = KEYWORD;
        return;
      }
    closeTag();
    if (v instanceof UnescapedData)
      {
        bout.clearWordEnd();
	bout.write(((UnescapedData) v).getData());
        prev = '-';
      }
    else if (v instanceof Char)
      Char.print(((Char) v).intValue(), this);
    else
      {
	startWord();
	prev = ' ';
	print(v);
        writeWordEnd();
	prev = WORD;
      }
  }

  /** Write each element of a sequence, which can be an array,
   * a Sequence or a Consumable. */
  //public void writeAll(Object sequence);

  /** True if consumer is ignoring rest of element.
   * The producer can use this information to skip ahead. */
  public boolean ignoring()
  {
    return false;
  }

  public void write (String str, int start, int length)
  {
    if (length > 0)
      {
        closeTag();
        int limit = start + length;
        int count = 0;
        while (start < limit)
          {
            char c = str.charAt(start++);
            if (mustHexEscape(c)
                || (inComment > 0 ? (c == '-' || inComment == 2)
                    : (c == '<' || c == '>' || c == '&'
                       || (inAttribute && (c == '"' || c < ' ' )))))
              {
                if (count > 0)
                  bout.write(str, start - 1 - count, count);
                write(c);
                count = 0;
              }
            else
              count++;
          }
        if (count > 0)
          bout.write(str, limit - count, count);
      }
    prev = '-';
  }

  public void write(char[] buf, int off, int len)
  {
    if (len > 0)
      {
        closeTag();
        int limit = off + len;
        int count = 0;
        while (off < limit)
          {
            char c = buf[off++];
            if (mustHexEscape(c)
                || (inComment > 0 ? (c == '-' || inComment == 2)
                    : (c == '<' || c == '>' || c == '&'
                       || (inAttribute && (c == '"' || c < ' ' )))))
              {
                if (count > 0)
                  bout.write(buf, off - 1 - count, count);
                write(c);
                count = 0;
              }
            else
              count++;
          }
        if (count > 0)
          bout.write(buf, limit - count, count);
      }
    prev = '-';
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    seq.consumeNext(ipos, this);
  }

  public void writeBaseUri (Object uri)
  {
  }

  public void beginComment ()
  {
    closeTag();
    if (printIndent >= 0)
      {
	if (prev == ELEMENT_START || prev == ELEMENT_END || prev == COMMENT)
	  writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
                     : PrettyWriter.NEWLINE_LINEAR);
      }
    bout.write("<!--");
    inComment = 1;
  }

  public void endComment ()
  {
    bout.write("-->");
    prev = COMMENT;
    inComment = 0;
  }

  public void writeComment(String chars)
  {
    beginComment();
    write(chars);
    endComment();
  }

  public void writeComment(char[] chars, int offset, int length)
  {
    beginComment();
    write(chars, offset, length);
    endComment();
  }

  public void writeCDATA (char[] chars, int offset, int length)
  {
    if (canonicalizeCDATA)
      {
        write(chars, offset, length);
        return;
      }
    closeTag();
    bout.write("<![CDATA[");
    int limit = offset+length;
    // Look for and deal with embedded "]]>".  This can't happen with
    // data generated from XML, but maybe somebody generated invalid CDATA.
    for (int i = offset;  i < limit - 2;  i++)
      {
	if (chars[i] == ']' && chars[i+1] == ']' && chars[i+2] == '>')
	  {
	    if (i > offset)
	      bout.write(chars, offset, i - offset);
	    print("]]]><![CDATA[]>");
	    offset = i + 3;
	    length = limit - offset;
	    i = i + 2;
	  }
      }
    bout.write(chars, offset, length);
    bout.write("]]>");
    prev = '>';
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    if ("xml".equals(target))
      needXMLdecl = false;
    closeTag();
    bout.write("<?");
    print(target);
    print(' ');
    bout.write(content, offset, length);
    bout.write("?>");
    prev = PROC_INST;
  }

  public void writePosition(SeqPosition position)
  {
    position.sequence.consumeNext(position.ipos, this);
  }

  public void error (String msg, String code)
  {
    throw new RuntimeException("serialization error: "+msg+" ["+code+']');
  }
}
