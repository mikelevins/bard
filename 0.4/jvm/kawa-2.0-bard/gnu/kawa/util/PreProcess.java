// Copyright (c) 2005, 2009, 2010  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;
import java.io.*;
import java.util.*;

/** Helper class to pre-process Java source. */

public class PreProcess
{
  // JAVA5:
  // Hashtable<String,Boolean> keywords = new Hashtable<String,Boolean>();
  Hashtable keywords = new Hashtable();

  String filename;
  int lineno;

  static final String JAVA4_FEATURES = "+JAVA2 +use:java.util.IdentityHashMap +use:java.lang.CharSequence +use:java.lang.Throwable.getCause +use:java.net.URI +use:java.util.regex +SAX2 +use:java.nio";
  static final String NO_JAVA4_FEATURES = "-JAVA5 -use:java.util.IdentityHashMap -use:java.lang.CharSequence -use:java.lang.Throwable.getCause -use:java.net.URI -use:java.util.regex -use:org.w3c.dom.Node -JAXP-1.3 -use:javax.xml.transform -JAVA5 -JAVA6 -JAVA6COMPAT5 -JAXP-QName -use:java.text.Normalizer -use:javax.lang.model -SAX2 -use:java.nio -Android";
  static final String JAVA5_FEATURES = "+JAVA5 "+JAVA4_FEATURES+" +use:org.w3c.dom.Node +use:javax.xml.transform +JAXP-1.3 -JAXP-QName";
  static final String NO_JAVA6_FEATURES = "-JAVA6 -JAVA7 -JAVA8 -use:java.lang.invoke -use:java.text.Normalizer -use:javax.lang.model";

  static String[] version_features = {
    "java1", "-JAVA2 "+NO_JAVA4_FEATURES+" "+NO_JAVA6_FEATURES,
    "java2", "+JAVA2 "+NO_JAVA4_FEATURES+" "+NO_JAVA6_FEATURES,
    // We don't use Node for java4 because there are inconsistencies between
    // the version of DOM used in Java4 and the one in Java 5 (and GCJ).
    "java4", "-JAVA5 "+JAVA4_FEATURES+" -use:org.w3c.dom.Node -JAXP-1.3 -use:javax.xml.transform -JAXP-QName -JAVA6COMPAT5 -Android "+NO_JAVA6_FEATURES,
    "java4x", "-JAVA5 "+JAVA4_FEATURES+" +use:org.w3c.dom.Node +JAXP-1.3 +use:javax.xml.transform -JAXP-QName -JAVA6COMPAT5 -Android "+NO_JAVA6_FEATURES,
    "java5", JAVA5_FEATURES+" -JAVA6COMPAT5 -Android "+NO_JAVA6_FEATURES,
    "java6compat5", JAVA5_FEATURES+" -JAVA6 -JAVA7 -JAVA8 +JAVA6COMPAT5 +use:java.text.Normalizer -use:javax.lang.model -use:java.lang.invoke -Android",
    "java6", JAVA5_FEATURES+" +JAVA6 -JAVA7 -JAVA8 -JAVA6COMPAT5 +use:java.text.Normalizer +use:javax.lang.model -use:java.lang.invoke -Android",
    "java7", JAVA5_FEATURES+" +JAVA6 +JAVA7 -JAVA8 -JAVA6COMPAT5 +use:java.text.Normalizer +use:javax.lang.model +use:java.lang.invoke -Android",
    "java8", JAVA5_FEATURES+" +JAVA6 +JAVA7 +JAVA8 -JAVA6COMPAT5 +use:java.text.Normalizer +use:javax.lang.model +use:java.lang.invoke -Android",
    "android", "+JAVA5 "+JAVA4_FEATURES+" +use:org.w3c.dom.Node +JAXP-1.3 -JAXP-QName -use:javax.xml.transform -JAVA6 -JAVA6COMPAT5 +Android "+NO_JAVA6_FEATURES,
  };

  void error(String msg)
  {
    System.err.println(filename+':'+lineno+": "+msg);
    System.exit(-1);
  }

  byte[] resultBuffer;
  int resultLength;

  public void filter (String filename) throws Throwable
  {
    if (filter(filename, new BufferedInputStream(new FileInputStream(filename))))
      {
	FileOutputStream out = new FileOutputStream(filename);
	out.write(resultBuffer, 0, resultLength);
	out.close();
	System.err.println("Pre-processed "+filename);
      }
  }

  public boolean filter (String filename, BufferedInputStream in) throws Throwable
  {
    this.filename = filename;
    boolean changed = false;
    
    byte[] buf = new byte[2000];
    int len = 0;
    int lineStart = 0;
    int dataStart = -1;
    int cmdLine= 0;
    lineno = 1;
    // If non-negative, comment out at this indentation.
    int commentAt = -1;
    int curIndent = 0;
    int nesting = 0;
    // If currently skipping, the nesting level of the controlling
    // conditional.  Otherwise, 0.
    int skipNesting = 0;
    String cmd = null;
    int changedLine = 0; // -1: comment added or moved; +1 comment removed
    for (;;)
      {
	int c = in.read();
	if (c < 0)
	  break;
	if (len + 10 >= buf.length) // Allow a little extra for look-ahead.
	  {
	    byte[] nbuf = new byte[2 * len];
	    System.arraycopy(buf, 0, nbuf, 0, len);
	    buf = nbuf;
	  }
	if (commentAt >= 0 && dataStart < 0 && changedLine <= 0
	    && c != '\r' && c != '\n'
	    && (commentAt == curIndent
		|| (c != ' ' && c != '\t')))
	  {
	    boolean doComment;
	    if (c == '/')
	      {
		// This is a little tricky.  We want to comment out regular
		// comments, because they might continue over multiple lines,
		// or because they might be documentation comments (which
		// we want to comment out so javadoc doesn't see them).
		// However, we don't want to comment out directives.
		in.mark(100);
		int d = in.read();
		if (d == '/')
		  doComment = false;
		else if (d == '*')
		  {
		    do { d = in.read(); } while (d == ' ' || d == '\t');
		    doComment = d != '#';
		  }
		else
		  doComment = true;
		in.reset();
	      }
	    else
	      doComment = true;
	    if (doComment)
	      {
		buf[len++] = '/';
		buf[len++] = '/';
		buf[len++] = ' ';
		changedLine = 1;
                changed = true;
	      }
	  }
	if (c != ' ' && c != '\t' && dataStart < 0)
	  {
	    // First non-space character.
	    dataStart = len;
	    if (nesting > 0 && commentAt != curIndent && c == '/')
	      {
		c = in.read();
		if (c < 0)
		  break;
		else if (c != '/')
		  buf[len++] = '/';
		else
		  {
		    c = in.read();
		    if (c < 0)
		      break;
		    changedLine = -1;
                    changed = true;
		    if (c == ' ')
                      {
                        c = in.read();
                        if (c == ' ' || c == '\t')
                          dataStart = -1;
                      }
		  }
	      }
	  }
	buf[len] = (byte) c;
	len++;
	if (c == '\n')
	  {
	    int firstNonSpace = -1;
	    int lastNonSpace = 0;
	    for (int i = lineStart; i < len-1; i++)
	      {
		if (buf[i] != ' ' && buf[i] != '\t' && buf[i] != '\r')
		  {
		    lastNonSpace = i;
		    if (firstNonSpace < 0)
		      firstNonSpace = i;
		  }
	      }
	    if (lastNonSpace - firstNonSpace >= 4
		&& buf[firstNonSpace] == '/'
		&& buf[firstNonSpace+1] == '*'
		&& buf[lastNonSpace-1] == '*'
		&& buf[lastNonSpace] == '/')
	      {
		firstNonSpace += 2;
		while (firstNonSpace < lastNonSpace
		       && buf[firstNonSpace] == ' ')
		  firstNonSpace++;
		lastNonSpace -= 2;
		while (lastNonSpace > firstNonSpace
		       && buf[lastNonSpace] == ' ')
		  lastNonSpace--;
		if (buf[firstNonSpace] == '#')
		  {
		    String cmnt = new String(buf, firstNonSpace,
					     lastNonSpace - firstNonSpace + 1,
					     "ISO-8859-1");
		    int sp = cmnt.indexOf(' ');
		    String rest;
		    Object binding;
		    cmdLine = lineno;
		    if (sp > 0)
		      {
			cmd = cmnt.substring(0, sp);
			rest = cmnt.substring(sp).trim();
			binding = keywords.get(rest);
		      }
		    else
		      {
			cmd = cmnt;
			rest = "";
			binding = null;
		      }
		    if ("#ifdef".equals(cmd) || "#ifndef".equals(cmd))
		      {
			if (binding == null)
			  {
			    System.err.println(filename+":"+lineno
					       +": warning - undefined keyword: "+rest);
			    binding = Boolean.FALSE;
			  } 
			nesting++;
                        if (skipNesting > 0)
                          commentAt = curIndent;
                        else if ((cmd.charAt(3) == 'n')
                                 != (binding == Boolean.FALSE))
                          {
                            commentAt = curIndent;
                            skipNesting = nesting;
                          }
		      }
		    else if ("#else".equals(cmd))
		      {
			if (nesting == 0)
			  error("unexpected "+cmd);
                        else if (nesting == skipNesting)
                          {
                            commentAt = -1;
                            skipNesting = 0;
                          }
                        else
                          {
                            commentAt = curIndent;
                            if (skipNesting == 0)
                              skipNesting = nesting;
                          }
		      }
		    else if ("#endif".equals(cmd))
		      {
			if (nesting == 0)
			  error("unexpected "+cmd);
                        if (nesting == skipNesting)
                          {
                            skipNesting = 0;
                            commentAt = -1;
                          }
                        else if (skipNesting > 0)
                          commentAt = curIndent;
			nesting--;
		      }
		    else
		      error("unknown command: "+cmnt);
		  }
	      }
	    lineStart = len;
	    dataStart = -1;
	    curIndent = 0;
	    lineno++;
	    changedLine = 0;
	  }
	else if (dataStart < 0)
	  curIndent = c == '\t' ? (curIndent + 8) & ~7 : curIndent + 1;
      }
    if (nesting != 0)
      {
	lineno = cmdLine;
	error("unterminated "+cmd);
      }
    resultBuffer = buf;
    resultLength = len;
    return changed;
  }

  void handleArg (String arg)
  {
    if (arg.charAt(0) == '%')
      {
        arg = arg.substring(1);
        for (int i = 0;  ;  i += 2 )
          {
            if (i >= version_features.length)
              {
                System.err.println("Unknown version: "+arg);
                System.exit(-1);
              }
            if (arg.equals(version_features[i]))
              {
                String features = version_features[i+1];
                System.err.println("(variant "+arg+" maps to: "+features+")");
                StringTokenizer tokenizer = new StringTokenizer(features);
                while (tokenizer.hasMoreTokens())
                  handleArg(tokenizer.nextToken());
                break;
              }
          }
      }
    else if (arg.charAt(0) == '+')
      keywords.put(arg.substring(1), Boolean.TRUE);
    else if (arg.charAt(0) == '-')
      {
        int eq = arg.indexOf('=');
        if (eq > 1)
          {
            String keyword
              = arg.substring(arg.charAt(1) == '-' ? 2 :1, eq);
            String value = arg.substring(eq+1);
            Boolean b = Boolean.FALSE;
            if (value.equalsIgnoreCase("true"))
              b = Boolean.TRUE;
            else if (! value.equalsIgnoreCase("false"))
              {
                System.err.println("invalid value "+value+" for "+keyword);
                System.exit(-1);
              }
            keywords.put(keyword, b);
          }
        else
          keywords.put(arg.substring(1), Boolean.FALSE);
      }
    else
      {
        try
          {
            filter(arg);
          }
        catch (Throwable ex)
          {
            System.err.println("caught "+ex);
            ex.printStackTrace();
            System.exit(-1);
          }
      }
  }

  public static void main (String[] args)
  {
    PreProcess pp = new PreProcess();
 
    pp.keywords.put("true", Boolean.TRUE);
    pp.keywords.put("false", Boolean.FALSE);

    for (int i = 0;  i < args.length;  i++)
      pp.handleArg(args[i]);
  }
}
