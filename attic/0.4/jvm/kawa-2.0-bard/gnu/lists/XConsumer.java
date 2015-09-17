// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer extended with XML-specific methods.
 * This should probably be in gnu.xml, but that complications TreeList. FIXME.
 */

public interface XConsumer extends Consumer
                           // Maybe future: extends org.xml.sax.ContentHandler
{
  public void writeComment(char[] chars, int offset, int length);

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length);

  public void writeCDATA(char[] chars, int offset, int length);

  public void beginEntity (Object baseUri);
  public void endEntity ();

  // Maybe future?
  // public void setDocumentLocator(org.xml.sax.Locator locator);
}
