// Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.kawa.io.Path;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import gnu.xml.*;
import java.net.URL;
import java.lang.ref.*;
import java.util.*;

/** Implement the XQuery function 'document'. */

public class Document
{
  public static final Document document = new Document();

  public static void parse (Object name, Consumer out) throws Throwable
  {
    SourceMessages messages = new SourceMessages();
    if (out instanceof XConsumer)
      ((XConsumer) out).beginEntity(name);
    gnu.xml.XMLParser.parse(name, messages, out);
    if (messages.seenErrors())
      throw new SyntaxException("document function read invalid XML",
				messages);
    if (out instanceof XConsumer)
      ((XConsumer) out).endEntity();
  }

  public static KDocument parse (Object uri) throws Throwable
  {
    NodeTree tree = new NodeTree();
    parse(uri, (Consumer) tree);
    return new KDocument(tree, TreeList.BEGIN_ENTITY_SIZE << 1);
  }

  private static ThreadLocal<HashMap<Path,KDocument>> docMapLocation
    = new ThreadLocal<HashMap<Path,KDocument>>();

  /* #ifdef JAVA2 */
  private static HashMap cache
    = new HashMap();

  private static class DocReference extends SoftReference
  { 
    static ReferenceQueue queue = new ReferenceQueue();
    Path key;
    public DocReference (Path key, KDocument doc)
      {
        super(doc, queue);
        this.key = key;
      }
    }
  /* #endif */

  /** Clear the thread-local uri-to-document map. */
  public static void clearLocalCache ()
  {
    docMapLocation.set(null);
  }

  /** Clear the global uri-to-document "soft" cache. */
  public static void clearSoftCache ()
  {
    /* #ifdef JAVA2 */
    cache = new HashMap();
    /* #endif */
  }

  public static KDocument parseCached (Object uri)
    throws Throwable
  {
    return parseCached(Path.valueOf(uri));
  }

  public static synchronized KDocument parseCached (Path uri)
    throws Throwable
  {
    /* #ifdef JAVA2 */
    for (;;)
      {
        DocReference oldref = (DocReference) DocReference.queue.poll();
        if (oldref == null)
          break;
        cache.remove(oldref.key);
      }
    /* #endif */
    HashMap<Path,KDocument> map = docMapLocation.get();
    if (map == null)
      {
        map = new HashMap<Path,KDocument>();
        docMapLocation.set(map);
      }
    KDocument doc = map.get(uri);
    if (doc != null)
      return doc;
    /* #ifdef JAVA2 */
    DocReference ref = (DocReference) cache.get(uri);
    if (ref != null)
      {
        doc = (KDocument) ref.get();
        if (doc == null)
          cache.remove(uri);
        else
          {
            map.put(uri, doc);
            return doc;
          }
      }
    /* #endif */
    doc = parse(uri);
    map.put(uri, doc);
    /* #ifdef JAVA2 */
    cache.put(uri, new DocReference(uri, doc));
    /* #endif */
    return doc;
  }
}
