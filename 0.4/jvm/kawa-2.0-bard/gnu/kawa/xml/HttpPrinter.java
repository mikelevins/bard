// Copyright (c) 2002, 2006, 2010  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import gnu.mapping.*;
import gnu.kawa.io.OutPort;
import java.io.*;
import java.util.Vector;

/** Output as an Http response.
 * Used for both CGI scripts (default) and HttpServletResponse (future).
 */

public class HttpPrinter extends FilterConsumer
{
  Vector headers = new Vector();
  /** Used as output buffer if base is null. */
  StringBuilder sbuf = new StringBuilder(100);
  Object currentHeader;

  /** 1 - implicit; 2: explicit. */
  private int seenStartDocument;

  protected String sawContentType;

  /** Difference between number of startElement and endElement calls so far. */
  private int elementNesting;

  protected OutputStream ostream;
  OutPort writer;

  public HttpPrinter(OutputStream out)
  {
    super(null);
    ostream = out;
  }

  public HttpPrinter(OutPort out)
  {
    super(null);
    writer = out;
  }

  public static HttpPrinter make (OutPort out)
  {
    return new HttpPrinter(out);
  }

  private void writeRaw(String str)
    throws java.io.IOException
  {
    if (writer != null)
      writer.write(str);
    else
      {
	int len = str.length();
	for (int i = 0;  i < len;  i++)
	  ostream.write((byte) str.charAt(i));
      }
  }

  protected void beforeNode ()
  {
    if (sawContentType == null)
      addHeader("Content-type", "text/xml");
    beginData();
  }

  public void printHeader(String label, String value)
    throws java.io.IOException
  {
    writeRaw(label);
    writeRaw(": ");
    writeRaw(value); // FIXME - need to quote?
    writeRaw("\n");
  }

  public void printHeaders()
    throws java.io.IOException
  {
    int num = headers.size();
    for (int i = 0;  i < num;  i += 2)
      printHeader(headers.elementAt(i).toString(),
		  headers.elementAt(i + 1).toString());
    writeRaw("\n");
  }

  public void addHeader(String label, String value)
  {
    if (label.equalsIgnoreCase("Content-type"))
      sawContentType = value;
    headers.addElement(label);
    headers.addElement(value);
  }

  public void startAttribute(Object attrType)
  {
    if (base == null)
      currentHeader = attrType;
    else
      base.startAttribute(attrType);
  }

  public void endAttribute()
  {
    if (currentHeader != null)
      {
	addHeader(currentHeader.toString(), sbuf.toString());
	sbuf.setLength(0);
	currentHeader = null;
      }
    else
      base.endAttribute();
  }

  boolean seenXmlHeader;

  public void beginData()
  {
    if (base == null)
      {
	if (sawContentType == null)
	  addHeader("Content-type", "text/plain");
	if (writer == null)
	  writer = new OutPort(ostream); // FIXME use encoding.
	String style = null;
	if ("text/html".equalsIgnoreCase(sawContentType))
	  style = "html";
	else if ("application/xhtml+xml".equalsIgnoreCase(sawContentType))
	  style = "xhtml";
	else if ("text/plain".equalsIgnoreCase(sawContentType))
	  style = "plain";
	base = XMLPrinter.make(writer, style);
        if (seenStartDocument == 0)
          {
            base.startDocument();
            seenStartDocument = 1;
          }
	try
	  {
	    printHeaders();
	  }
	catch (Exception ex)
	  {
	    throw new RuntimeException(ex);
	  }
      }
    /* #ifdef use:java.lang.CharSequence */
    append(sbuf);
    /* #else */
    // write(sbuf.toString());
    /* #endif */
    sbuf.setLength(0);
  }

  public void startElement (Object type)
  {
    if (sawContentType == null)
      {
	String mimeType;
	if (! seenXmlHeader)
	  mimeType = "text/html";
	else if (type instanceof Symbol
                 && "html".equals(((Symbol) type).getLocalPart()))
	  mimeType = "text/xhtml";
	else
	  mimeType = "text/xml";
	addHeader("Content-type", mimeType);
      }
    beginData();
    base.startElement(type);
    elementNesting++;
  }

  public void endElement ()
  {
    super.endElement();
    elementNesting--;
    if (elementNesting == 0 && seenStartDocument == 1)
      endDocument();
  }

  public void writeObject(Object v)
  {
    if (v instanceof Consumable && ! (v instanceof UnescapedData))
      ((Consumable) v).consume(this);
    else
      {
	beginData();
	super.writeObject(v);
      }
  }

  /* #ifdef use:java.lang.CharSequence */
  public void write (CharSequence str, int start, int length)
  /* #else */
  // public void write (String str, int start, int length)
  /* #endif */
  {
    if (base == null)
      sbuf.append(str, start, start+length);
    else
      base.write(str, start, length);
  }

  public void write(char[] buf, int off, int len)
  {
    if (base == null)
      sbuf.append(buf, off, len);
    else
      base.write(buf, off, len);
  }

  public void startDocument()
  {
    if (base != null)
      base.startDocument();
    seenStartDocument = 2;
  }

  public void endDocument()
  {
    if (base != null)
      base.endDocument();
    try
      {
        if (sawContentType == null)
          addHeader("Content-type", "text/plain");
        if (sbuf.length() > 0)
          {
            String str = sbuf.toString();
            sbuf.setLength(0);
            if (writer != null)
              writer.write(str);
            else
              ostream.write(str.getBytes());
          }
	// else ???;
	if (writer != null)
	  writer.close();
	if (ostream != null)
	  ostream.flush();
      }
    catch (Exception ex)
      {
      }
  }

  /** Try to reset (delete) any response generated so far.
   * @param headersAlso if response headers should also be reset.
   * @return true on success, false if it's too late.
   */
  public boolean reset (boolean headersAlso)
  {
    if (headersAlso)
      {
        headers.clear();
        sawContentType = null;
        currentHeader = null;
        elementNesting = 0;
      }
    sbuf.setLength(0);
    base = null;
    boolean ok = true;
    if (ostream != null)
      {
        ok = writer == null;
        writer = null;
      }
    return ok;
  }
}
