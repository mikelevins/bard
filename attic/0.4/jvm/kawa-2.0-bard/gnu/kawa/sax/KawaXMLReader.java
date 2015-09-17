// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.sax;
import gnu.kawa.io.InPort;
import gnu.xml.*;
import gnu.text.*;
import java.io.*;
import org.xml.sax.*;

/** An implementation of SAX2 XMLReader.
 */

public class KawaXMLReader extends ContentConsumer
  implements XMLReader
{
  public boolean getFeature (String name)
  {
    return false;
  }

  public void setFeature (String name, boolean value)
  {
  }

  public Object getProperty (String name)
  {
    return null;
  }

  public void setProperty (String name, Object value)
  {
  }

  public void setEntityResolver (EntityResolver resolver)
  {
  }

  public EntityResolver getEntityResolver ()
  {
    return null;
  }

  public void setDTDHandler (DTDHandler handler)
  {
  }

  public DTDHandler getDTDHandler ()
  {
    return null;
  }

  ErrorHandler errorHandler;


  public void setErrorHandler (ErrorHandler handler)
  {
    errorHandler = handler;
  }

  public ErrorHandler getErrorHandler ()
  {
    return errorHandler;
  }

  public void parse (InputSource input)
    throws IOException, SAXException
  {
    Reader reader = input.getCharacterStream();
    if (reader == null)
      reader = XMLParser.XMLStreamReader(input.getByteStream());
    SourceMessages messages = new SourceMessages();
    gnu.xml.XMLFilter filter = new gnu.xml.XMLFilter(this);
    InPort lin = new InPort(reader);
    filter.setSourceLocator(lin);
    getContentHandler().setDocumentLocator(filter);
    XMLParser.parse(lin, messages, filter);
    String err = messages.toString(20);
    if (err != null)
      throw new SAXParseException(err, filter);
  }

  public void parse (String systemId)
  {
  }
}
