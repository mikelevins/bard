// Copyright (c) 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.kawa.xml.KNode;
import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.Path;
import gnu.kawa.io.URIPath;
import gnu.xml.XName;
import gnu.kawa.xml.UntypedAtomic;  // FIXME - bad cross-package dependency.
import gnu.kawa.xml.ElementType; // FIXME - bad cross-package dependency.

/** Use to represent a Document or Document Fragment, in the XML DOM sense.
 * More compact than traditional DOM, since it uses many fewer objects.
 */

public class NodeTree extends TreeList
{
  public int nextPos (int position)
  {
    boolean isAfter = (position & 1) != 0;
    int index = posToDataIndex(position);
    int next = nextNodeIndex(index, -1 >>> 1);
    if (next != index)
      return next << 1;
    if (index == data.length)
      return 0;
    return (index << 1) + 3;
  }

  public static NodeTree make ()
  {
    return new NodeTree();
  }

  static int counter;
  int id;

  /** Get/create a new unique number. */
  public int getId()
  {
    if (id == 0)
      id = ++counter;
    return id;
  }

  public int stableCompare (AbstractSequence other)
  {
    if (this == other)
      return 0;
    // If other is also a NodeTree it would be simpler to just compare
    // the results of getId, but if we always did that there is the
    // slight risk that counter could overflow in the case of a
    // long-running program.  So we use system.identityHashCode as
    // the primary "key" and getId only when needed as a tie-breaker.
    int comp = super.stableCompare(other);
    if (comp == 0 && other instanceof NodeTree)
      {
	int id1 = this.getId();
	int id2 = ((NodeTree) other).getId();
	comp = id1 < id2 ? -1 : id1 > id2 ? 1 : 0;
      }
    return comp;
  }

  public SeqPosition getIteratorAtPos(int ipos)
  {
    return KNode.make(this, ipos);
  }

  public String posNamespaceURI (int ipos)
  {
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).getNamespaceURI();
    if (type instanceof Symbol)
      return ((Symbol) type).getNamespaceURI();
    return null;
  }

  public String posPrefix (int ipos)
  {
    String name = getNextTypeName(ipos);
    if (name == null)
      return null;
    int colon = name.indexOf(':');
    return colon < 0 ? null : name.substring(0, colon);
  }

  public String posLocalName (int ipos)
  {
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).getLocalPart();
    if (type instanceof Symbol)
      return ((Symbol) type).getLocalName();
    return getNextTypeName(ipos);
  }

  public boolean posIsDefaultNamespace (int ipos, String namespaceURI)
  {
    throw new Error("posIsDefaultNamespace not implemented");
  }

  public String posLookupNamespaceURI (int ipos, String prefix)
  {
    int kind = getNextKind(ipos);
    if (kind != Sequence.ELEMENT_VALUE)
      throw new IllegalArgumentException("argument must be an element");
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).lookupNamespaceURI(prefix);
    else
      return null;
  }

  public String posLookupPrefix (int ipos, String namespaceURI)
  {
    throw new Error("posLookupPrefix not implemented");
  }

  public int posFirstChild(int ipos)
  {
    int index = gotoChildrenStart(posToDataIndex(ipos));
    if (index < 0)
      return -1;
    char datum = data[index];
    if (datum == END_ELEMENT_SHORT || datum == END_ELEMENT_LONG
	|| datum == END_DOCUMENT)
      return -1;
    return index << 1;
  }

  public boolean posHasAttributes (int ipos)
  {
    int index = gotoAttributesStart(posToDataIndex(ipos));
    if (index < 0)
      return false;
    return index >= 0 && data[index] == BEGIN_ATTRIBUTE_LONG;
  }

  /** Find named attribute.
   * @param namespaceURI need not be interned,
   *   or null which matches any namespace
   * @param localName need not be interned,
   *   or null which matches any local name
   * @return attribute ipos or 0
   */
  public int getAttribute (int parent, String namespaceURI, String localName)
  {
    return getAttributeI(parent,
                         namespaceURI == null ? null : namespaceURI.intern(),
                         localName == null ? null : localName.intern());
  }

  /** Find named attribute.
   * @param namespaceURI an interned String or null which matches any namespace
   * @param localName an interned String, or null which matches any local name
   * @return attribute ipos or 0
   */
  public int getAttributeI (int parent, String namespaceURI, String localName)
  {
    int attr = firstAttributePos(parent);
    for (;;)
      {
        if (attr == 0 || getNextKind(attr) != Sequence.ATTRIBUTE_VALUE)
          return 0;
        if ((localName == null || posLocalName(attr) == localName)
            && (namespaceURI == null || posNamespaceURI(attr) == namespaceURI))
          return attr;
        attr = nextPos(attr);
      }
  }

  /** Return the type-value of the node at the specified position. */
  public Object typedValue (int ipos)
  {
    // FIXME when we support validation.
    StringBuffer sbuf = new StringBuffer();
    stringValue(posToDataIndex(ipos), sbuf);
    String str = sbuf.toString();
    int kind = getNextKind(ipos);
    if (kind == Sequence.PROCESSING_INSTRUCTION_VALUE
        || kind == Sequence.COMMENT_VALUE)
      return str;
    return new UntypedAtomic(str);
  }

  /** Get the target of a process-instruction. */
  public String posTarget (int ipos)
  {
    int index = posToDataIndex(ipos);
    if (data[index] != PROCESSING_INSTRUCTION)
      throw new ClassCastException("expected process-instruction");
    return (String) objects[getIntN(index+1)];
  }

  /** Look for matching attribute in ancestor or self.
   * @param namespace namespaceURI (interned) of required attribute
   * @param name localName(interned) of required attribute 
   * @return attribute ipos or 0
   */
  public int ancestorAttribute (int ipos,
                                String namespace, String name)
  {
    for (;;)
      {
        if (ipos == -1)
          return 0;
        int attr = getAttributeI(ipos, namespace, name);
        if (attr != 0)
          return attr;
        ipos = parentPos(ipos);
      }
  }

  /** Return of the base-uri property, if known, of the node at pos. */
  public Path baseUriOfPos (int pos, boolean resolveRelative)
  {
    Path uri = null;
    int index = posToDataIndex(pos);
    for (;;)
      {
	if (index == data.length)
	  return null;
	char datum = data[index];
        Path base = null;
        if (datum == BEGIN_ENTITY)
          {
            int oindex = getIntN(index+1);
            if (oindex >= 0)
              base = URIPath.makeURI(objects[oindex]);
          }
	else if ((datum >= BEGIN_ELEMENT_SHORT
	     && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
	    || datum == BEGIN_ELEMENT_LONG)
          {
            int attr = getAttributeI(pos, NamespaceBinding.XML_NAMESPACE, "base");
            if (attr != 0)
              base = URIPath.valueOf(KNode.getNodeValue(this, attr));
          }
        if (base != null)
          {
            uri = uri == null || ! resolveRelative ? base : base.resolve(uri);
            if (uri.isAbsolute() || ! resolveRelative)
              return uri;
          }
	index = parentOrEntityI(index);
	if (index == -1)
          return uri;
        pos = index << 1;
      }
  }

  public String toString ()
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    XMLPrinter xp = new XMLPrinter(wr);
    consume(xp);
    wr.close();
    return wr.toString();
  }

  /** If non-null, a hash-table of ID names. */
  String[] idNames;
  /** If non-null, a mapping of element ipos values mapped by idNames.
   * If {@code idNames[i]} is non-null, then {@code idOffsets[i]} is the
   * ipos value of the first element that has the former as its ID property. */
  int[] idOffsets;
  /** Number of non-null entries in idNames. */
  int idCount;

  public void makeIDtableIfNeeded ()
  {
    if (idNames != null)
      return;
    // Force allocation - in case there are no xml:id nodes,
    // so we don't scan multiple times.
    int size = 64;
    idNames = new String[size];
    idOffsets = new int[size];
    int limit = endPos();
    int ipos = 0;
    for (;;)
      {
	ipos = nextMatching(ipos, ElementType.anyElement, limit, true);
	if (ipos == 0)
	  break;
        // Until we do validation, we only recognize 'xml:id' as setting
        // the is-id property.  FIXME.
        int attr = getAttributeI(ipos, NamespaceBinding.XML_NAMESPACE, "id");
        if (attr != 0)
          {
            enterID(KNode.getNodeValue(this, attr), ipos);
          }
      }
  }

  void enterID (String name, int offset)
  {
    int size;
    String[] tmpNames = idNames;
    int[] tmpOffsets = idOffsets;
    if (tmpNames == null)
      {
        size = 64;
        idNames = new String[size];
        idOffsets = new int[size];
      }
    else if (4 * idCount >= 3 * (size = idNames.length))
      {
        idNames = new String[2 * size];
        idOffsets = new int[2 * size];
        idCount = 0;
        for (int i = size;  --i >= 0; )
          {
            String oldName = tmpNames[i];
            if (oldName != null)
              enterID(oldName, tmpOffsets[i]);
          }
        tmpNames = idNames;
        tmpOffsets = idOffsets;
        size = 2 * size;
      }
    int hash = name.hashCode();
    int mask = size - 1;
    int index = hash & mask;
    // Must be odd - or more specifically relatively prime with size,
    int step = (~hash << 1) | 1;
    for (;;)
      {
        String oldName = tmpNames[index];
        if (oldName == null)
          {
            tmpNames[index] = name;
            tmpOffsets[index] = offset;
            break;
          }
        if (oldName.equals(name)) // intern and == ?? FIXME
          {
            // Nothing to do.
            return;
          }
        index = (index + step) & mask;
      }
    idCount++;
  }

  /** Look for an element with matching ID.
   * Returns an element ipos, or -1 if not found.
   * Since we don't do any validation, for now only attributes with the
   * name {@code xml:id} are recognized has having the {@code is-id} property.
   * Assumes makeIDtableIfNeeded has been called at soem point.
   */
  public int lookupID (String name)
  {
    String[] tmpNames = idNames;
    int[] tmpOffsets = idOffsets;
    int size = idNames.length;
    int hash = name.hashCode();
    int mask = size - 1;
    int index = hash & mask;
    // Must be odd - or more specifically relatively prime with size,
    int step = (~hash << 1) | 1;
    for (;;)
      {
        String oldName = tmpNames[index];
        if (oldName == null)
          return -1;
        if (oldName.equals(name)) // intern and == ?? FIXME
          {
            return tmpOffsets[index];
          }
        index = (index + step) & mask;
      }
  }
}
