// Copyright (c) 2002, 2010  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import gnu.kawa.xml.*;
import java.io.*;

/** A Consumer that sends output to an http-server's response stream.
 * A "response-header" object is handled specially.
 * In spite of the name, this is also used for non-servlet-based servers.
 */

public class ServletPrinter extends HttpPrinter
{
  HttpRequestContext hctx;

  public ServletPrinter(HttpRequestContext hctx, int bufSize)
    throws IOException
  {
    super(new HttpOutputStream(hctx, bufSize));
    this.hctx = hctx;
  }

  public void addHeader(String label, String value)
  {
    if (label.equalsIgnoreCase("Content-type"))
      {
	super.sawContentType = value;
	hctx.setContentType(value);
      }
    else if (label.equalsIgnoreCase("Status"))
      {
	int lval = value.length();
	int code = 0;
	int i;
	for (i = 0;  i < lval;  i++)
	  {
	    if (i >= lval)
	      {
                hctx.statusCode = code;
		break;
	      }
	    char ch = value.charAt(i);
	    int digit = Character.digit(ch, 10);
	    if (digit >= 0)
	      code = 10 * code + digit;
	    else
	      {
		if (ch == ' ')
		  i++;
                hctx.statusCode = code;
                hctx.statusReasonPhrase = value.substring(i);
		break;
	      }
	  }
      }
    else
      hctx.setResponseHeader(label, value);
  }

  public void printHeaders()
  {
  }

  public boolean reset (boolean headersAlso)
  {
    return ((HttpOutputStream) ostream).reset() & super.reset(headersAlso);
  }
}

class HttpOutputStream extends OutputStream
{
  HttpRequestContext context;
  byte[] buffer;
  OutputStream out;
  int count;

  public HttpOutputStream(HttpRequestContext context, int bufSize)
  {
    this.context = context;
    buffer = new byte[bufSize];
  }

  public void write(int b)
    throws IOException
  {
    if (count >= buffer.length)
      flush();
    buffer[count++] = (byte) b;
  }

  public void write(byte[] data, int offset, int length)
           throws IOException
  {
    int avail = buffer.length - count;
    while (length > avail)
      {
        System.arraycopy(data, offset, buffer, count, avail);
        count += avail;
        flush();
        offset += avail;
        length -= avail;
        avail = buffer.length;
      }
    if (length > 0)
      {
        System.arraycopy(data, offset, buffer, count, length);
        count += length;
      }
  }

  public boolean reset ()
  {
    count = 0;
    return out == null;
  }

  public void flush()
    throws IOException
  {
    if (out == null)
      {
        maybeSendResponseHeaders(-1);
        out = context.getResponseStream();
      }
    if (count > 0)
      {
        out.write(buffer, 0, count);
        count = 0;
      }
  }

  void maybeSendResponseHeaders (int count)
    throws IOException
  {
    int statusCode = context.statusCode;
    if (statusCode != HttpRequestContext.STATUS_SENT)
      {
        context.sendResponseHeaders(statusCode, context.statusReasonPhrase, count);
        context.statusCode = HttpRequestContext.STATUS_SENT;
      }
  }

  public void close() 
    throws IOException
  {
    if (out == null)
      {
        maybeSendResponseHeaders(count);
        out = context.getResponseStream();
      }
    flush();
    out.close();
  }
}
