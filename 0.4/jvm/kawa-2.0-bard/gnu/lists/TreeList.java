// Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import gnu.text.Char;

/** A compact representation of a tree, that is a nested list structure.
 * The data structure can store anything that can be emitted to a Consumer.
 * This data structure is optimized for efficient forwards traversal
 * through the data structure, not random access.
 * It does have an "insertion point"; insertions and deletions are
 * efficient through the use of a buffer gap.
 * It is a reasonable choice for a "DOM" for XML data.
 */

public class TreeList extends AbstractSequence<Object>
  implements
  /* #ifdef JAVA5 */
  Appendable,
  /* #endif */
  XConsumer, PositionConsumer, Consumable
{
  // Some public fields and methods are public which probably shouldn't be,
  // for the sake of XMLFilter.  FIXME.  Perhaps an abstract class
  // in gnu.lists that XMLFilter extend?

  public Object[] objects; 
  public int oindex;
  public char[] data;
  public int gapStart;
  public int gapEnd;

  /** If non-zero, gap is in an attribute starting (1 less than) here. */
  public int attrStart;
  /** If non-zero, gap is in an document starting (1 less than) here. */
  public int docStart;

  public TreeList()
  {
    resizeObjects();
    gapEnd = 200;
    data = new char[gapEnd];
  }

  /**
   * Make a copy of a sub-range of a TreeList.
   * @param list the TreeList to copy
   * @param startPosition start of range, as a raw index in data
   * @param endPosition end of range, as a raw index in data
   */
  public TreeList(TreeList list, int startPosition, int endPosition)
  {
    this();
    list.consumeIRange(startPosition, endPosition, this);
  }

  public TreeList(TreeList list)
  {
    this(list, 0, list.data.length);
  }

  public void clear()
  {
    gapStart = 0;
    gapEnd = data.length;
    attrStart = 0;
    if (gapEnd > 1500)
      {
	gapEnd = 200;
	data = new char[gapEnd];
      }
    objects = null;
    oindex = 0;
    resizeObjects();
  }

  // The data array contains an encoding of values, as follows:
  // 0x0000 ... 0x9FFF:  A single Unicode character.
  // 0xAXXX: BEGIN_ELEMENT_SHORT
  // 0xBXXX:  negative integer ((short)0x0XXX<<4)>>4, range -4096 to -1
  // 0xCXXX:  positive integer 0x0XXX, in the range 0 to 4095
  // 0xDXXX:  positive integer 0x1XXX, in the range 4096 to 8191
  // 0xEXXX:: OBJECT_REF_SHORT.  The object in objects[0xXXX].
  // 0xF0XX:  A byte (encoded character or char fragment) ((byte) 0xXX).
  // 0xF100 ... 0xF101:  A Boolean (BOOL_FALSE, BOOL_TRUE)
  // 0xF102 A B:      INT_FOLLOWS - 32-bit int (big-endian)
  // 0xF103 A B C D:  LONG_FOLLOWS 64-bit long int (big-endian)
  // 0xF104 A B:      FLOAT_FOLLOWS 32-bit float (big-endian)
  // 0xF105 A B C D:  DOUBLE_FOLLOWS 64-bit double (big-endian)
  // 0xF106: CHAR_FOLLOWS
  // 0xF108: BEGIN_ELEMENT_LONG
  // 0xF109: BEGIN_ATTRIBUTE_LONG
  // 0xF10A: END_ATTRIBUTE
  // 0xF10B: END_ELEMENT_SHORT
  // 0xF10C: END_ELEMENT_LONG
  // 0xF10D A B: OBJECT_REF_FOLLOWS:  The object in objects[(A,B)].
  // 0xF10E A B: POSITION_REF_FOLLOWS:  The TreePosition in objects[(A,B)].
  // 0xF10F A B C D: POSITION_PAIR_FOLLOWS
  // 0xF110 BEGIN_DOCUMENT
  // 0xF111 END_DOCUMENT
  // 0xF112 BEGIN_ENTITY
  // 0xF113 END_ENTITY
  // 0xF114 PROCESSING_INSTRUCTION
  // 0xF115 CDATA_SECTION
  // 0xF116 JOINER
  // 0xF117 COMMENT
  // 0xF118 DOCUMENT_URI: Not a node, but a property of the previous document.

  /** The largest Unicode character that can be encoded in one char. */
  public static final int MAX_CHAR_SHORT = 0x9FFF;

  /** The smallest integer that can use the short int encoding. */
  static final int MIN_INT_SHORT = -0x1000;  // -4096

  /** The largest integer that can use the short int encoding. */
  static final int MAX_INT_SHORT = 0x1FFF;  // 8191

  /** The value used to encode the integer zero. */
  static final int INT_SHORT_ZERO = 0xC000;

  /** The value used to encode the object in objects[0]. */
  static final int OBJECT_REF_SHORT = 0xE000;

  /** The maximum offset in the objects array for a short object-ref. */
  static final int OBJECT_REF_SHORT_INDEX_MAX = 0xFFF;

  /** Followed by 2 chars that provide an index into objects. */
  static final char OBJECT_REF_FOLLOWS = 0xF10D;

  /** Followed by 2 chars that provide an index into objects. */
  static final char POSITION_REF_FOLLOWS = 0xF10E;

  /** A position triple referencing some other "nodes".
   * Followed by index of sequence (2 chars), and ipos (2 chars). */
  protected static final char POSITION_PAIR_FOLLOWS = 0xF10F;

  /** Encoding prefix that indicates a byte value. */
  static final int BYTE_PREFIX = 0xF000;

  /** The value used to encode false. */
  static final char BOOL_FALSE = 0xF100;

  /** The value used to encode true. */
  static final char BOOL_TRUE = 0xF101;

  /** A 32-bit integer, non-compact form.
   *
   * [INT_FOLLOWS]
   * [word1], [word2]:  The big-endian bits of the integer.
   */
  public static final int INT_FOLLOWS = 0xF102;

  /** A 64-bit long integer.
   *
   * [LONG_FOLLOWS]
   * [word1], [word2], [word3], [word4]:  The big-endian bits of the long.
   */
  static final int LONG_FOLLOWS = 0xF103;

  /** A 32-bit float floating-pointer number.
   *
   * [FLOAT_FOLLOWS]
   * [word1], [word2]:  The big-endian bits of the float.
   */
  static final int FLOAT_FOLLOWS = 0xF104;

  /** A 64-bit double floating-pointer number.
   *
   * [DOUBLE_FOLLOWS]
   * [word1], [word2], [word3], [word4]:  The big-endian bits of the double.
   */
  static final int DOUBLE_FOLLOWS = 0xF105;

  /** A 16-bit (non-compact) Unicode char follows. */
  static final int CHAR_FOLLOWS = 0xF106;

  /** A comment node follows.
   * [COMMENT]
   * [length] 2 shorts
   * [comment text], (length) number of characters.
   */
  static final int COMMENT = 0xF117;

  /** A processing-instruction node follows.
   * [PROCESSING_INSTRUCTION]
   * [target] 2 shorts, where objects[target] is the target as a String.
   * [length] 2 shorts.
   * [comment text], (length) number of characters.
   */
  protected static final int PROCESSING_INSTRUCTION = 0xF114;

  /** A CDATA node follows.
   * [CDATA_SECTION]
   * [length] 2 shorts
   * [comment text], (length) number of characters.
   */
  static final int CDATA_SECTION = 0xF115;

  /** Suppress spacing between non-node items. */
  static final int JOINER = 0xF116;

  /** The beginning of an attribute.
   * [BEGIN_ATTRIBUTE_LONG]
   * [index], 2 shorts, where objects[index] is the attribute type name
   *   and objects[index+1] is the attribute type object.
   * [end_offset], 2 shorts, giving the location of the following
   *   END_ATTRIBUTE.  If the attribute straddles the gap, then
   *   end_offset is a negative offset relative to data.length.
   *   (Therefore allocating more space for the gap does not require
   *   adjusting end_offset.)  Otherwise, the end_offset is relative
   *   to the BEGIN_ATTRIBUTE_LONG word.
   */
  protected static final int BEGIN_ATTRIBUTE_LONG = 0xF109;
  public static final int BEGIN_ATTRIBUTE_LONG_SIZE = 5;

  /** The end of an attribute of a node. */
  static final int END_ATTRIBUTE = 0xF10A;
  public static final int END_ATTRIBUTE_SIZE = 1;

  /** Beginning of a document (or top-level value).
   * Used to distinguish a document from its element node.
   * [end_offset], 2 shorts, giving the location of the following
   *   END_DOCUMENT.  If the attribute straddles the gap, then
   *   end_offset is a negative offset relative to data.length.
   *   (Therefore allocating more space for the gap does not require
   *   adjusting end_offset.)  Otherwise, the end_offset is relative
   *   to the BEGIN_DOCUMENT word.
   * [parent_offset], in 2 shorts.  The parent node, or -1 if no parent.
   * Otherwise, a negative value is a relative offset, while a non-negative
   * value is absolute.  (Use the latter when gap is between this node and
   * its parent.  The parent would normally be a BEGIN_ENTITY.
   */
  protected static final int BEGIN_DOCUMENT = 0xF110;

  /** End of a document. */
  protected static final int END_DOCUMENT = 0xF111;

  /** Start of an entity (typically a file, possibly included).
   * [base_uri], 2 shorts, given an index of a base-uri object
   * [parent_offset], in 2 shorts, encoded as for BEGIN_DOCUMENT.
   */
  public static final int BEGIN_ENTITY = 0xF112;
  public static final int BEGIN_ENTITY_SIZE = 5;

  protected static final int END_ENTITY = 0xF113;

  /** The document-uri property of a node.
   * This is not an actual value, but it is a property of the previous
   * document node, or the surrounding node just after a BEGIN_XXX entry.
   * [DOCUMENT_URI]
   * [index]. 2 shorts, where objects[index] is the document-uri value.
   */
  protected static final int DOCUMENT_URI = 0xF118;

  /** Beginning of an element, compact form.
   *
   * [BEGIN_ELEMENT_SHORT + index], where objects[index] is the element's
   *   type name and objects[index+1] is the type object.
   * [end_offset], the unsigned offset (from the initial word)
   *   to the corresponding END_ELEMENT_SHORT.
   * [parent_offset], the (unsigned absolute value of the) offset
   *   to the outer BEGIN_ELEMENT_SHORT/BEGIN_ELEMENT_LONG/BEGIN_DOCUMENT.
   *.  (If these is no parent, then parent_offset==0.)
   *
   * This should is used when index < BEGIN_ELEMENT_SHORT_INDEX_MAX,
   * both end_offset and parent_offset fit in 16 bits,
   * and the element does not straddle the gap.
   */
  protected static final int BEGIN_ELEMENT_SHORT = 0xA000;
  protected static final int BEGIN_ELEMENT_SHORT_INDEX_MAX = 0xFFF;

  /** End of an element, compact form.
   *
   * [END_ELEMENT_SHORT]
   * [begin_offset], the unsigned absolute value of the offset to the
   *   matching BEGIN.  (This is the same as the matching end_offset.)
   *
   */
  protected static final int END_ELEMENT_SHORT = 0xF10B;

  /** Begin of an element, non-compact form.
   *
   * [BEGIN_ELEMENT_LONG]
   * [end_offset], in 2 shorts.  The position of the matching END_ELEMENT_LONG.
   *   If the element straddles the gap, then end_offset is a negative offset
   *   relative to data.length.  (Therefore allocating more space for the
   *   gap does not require adjusting any end_offset.)   If the element and
   *   and its children are all on the same side of the gap, then end_offset
   *   is a positive offset relative to the BEGIN_ELEMENT_LONG word.  (Hence
   *   shifting an entire element when the gap is moved does not require
   *   changing its end_offset.)
   *
   * Note that the space taken by a BEGIN_ELEMENT_LONG is the same that
   * needed for a BEGIN_ELEMENT_SHORT (but a END_ELEMENT_LONG takes much
   * more space than a END_ELEMENT_SHORT).  This is to make it easier
   * to convert a BEGIN_ELEMENT_LONG to a BEGIN_ELEMENT_SHORT or vice
   * versa, as needed.
   */
  protected static final int BEGIN_ELEMENT_LONG =  0xF108;

  /** End of n element, non-compact form.
   *
   * [END_ELEMENT_LONG]
   * [index], 2 shorts where objects[index] is the element's type name and
   *   objects[index+1] is the type object.
   * [begin_offset], in 2 shorts.  The position of the matching
   *   BEGIN_ELEMENT_LONG.  If the element straddles the gap, then begin_offset
   *   is the actual index (i.e. relative to the start of data) of the
   *   matching BEGIN_ELEMENT_LONG.  (Therefore allocating more space for the
   *   gap does not require adjusting begin_offset.)  If the element does not
   *   straddle the gap, then begin_offset is a negative offset relative
   *   to the END_ELEMENT_LONG word.  (Hence shifting an entire element when
   *   the gap is moved does not require changing its begin_offset.)
   *   relative to data.length.
   * [parent_offset], in 2 shorts.  The position of the outer BEGIN_ELEMENT_LONG,
   *   BEGIN_ELEMENT_SHORT or BEGIN_DOCUMENT.  If the difference straddles
   *   the gap (i.e. either this element straddles the gap or the parent element
   *   does and the gap precedes this element), then parent_offset is the
   *   actual index of the parent element.  Otherwise, then parent_offset is a
   *   negative offset relative to the END_ELEMENT_LONG word.
   */
  protected static final int END_ELEMENT_LONG = 0xF10C;

  int currentParent = -1;

  public void ensureSpace(int needed)
  {
    int avail = gapEnd - gapStart;
    if (needed > avail)
      {
	int oldSize = data.length;
	int neededSize = oldSize - avail + needed;
	int newSize = 2 * oldSize;
	if (newSize < neededSize)
	  newSize = neededSize;
	char[] tmp = new char[newSize];
	if (gapStart > 0)
	  System.arraycopy(data, 0, tmp, 0, gapStart);
	int afterGap = oldSize - gapEnd;
	if (afterGap > 0)
	  System.arraycopy(data, gapEnd, tmp, newSize - afterGap, afterGap);
	gapEnd = newSize - afterGap;
	data = tmp;
      }
  }

  public final void resizeObjects()
  {
    int oldLength;
    int newLength;
    Object[] tmp;
    if (objects == null)
      {
	oldLength = 0;
	newLength = 100;
	tmp = new Object[newLength];
      }
    else
      {
	oldLength = objects.length;
	newLength = 2 * oldLength;
	tmp = new Object[newLength];
	System.arraycopy(objects, 0, tmp, 0, oldLength);
      }
    objects = tmp;
  }

  public int find (Object arg1)
  {
    if (oindex == objects.length)
      resizeObjects();
    objects[oindex] = arg1;
    return oindex++;
  }

  /** Get a 32-bit int from the data array. */
  final protected int getIntN(int index)
  {
    return (data[index] << 16) | (data[index + 1] & 0xFFFF);
  }

  /** Get a 64-bit long from the data array. */
  final protected long getLongN(int index)
  {
    char[] data = this.data; // Optimization.
    return (data[index] & 0xFFFFL) << 48
      | (data[index+1] & 0xFFFFL) << 32
      | (data[index+2] & 0xFFFFL) << 16
      | (data[index+3] & 0xFFFFL);
  }

  final public void setIntN(int index, int i)
  {
    data[index] = (char) (i >> 16);
    data[index+1] = (char) i;
  }

  public void writePosition(SeqPosition position)
  {
    ensureSpace(3);
    // FIXME - no need for find to search in this case!
    int index = find(position.copy());
    data[gapStart++] = POSITION_REF_FOLLOWS;
    setIntN(gapStart, index);
    gapStart += 2;
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    ensureSpace(5);
    data[gapStart] = POSITION_PAIR_FOLLOWS;
    int seq_index = find(seq);
    setIntN(gapStart+1, seq_index);
    setIntN(gapStart+3, ipos);
    gapStart += 5;
  }

  public void writeObject(Object v)
  {
    ensureSpace(3);
    int index = find(v);
    if (index < 0x1000)
      data[gapStart++] = (char) (OBJECT_REF_SHORT | index);
    else
      {
	data[gapStart++] = OBJECT_REF_FOLLOWS;
	setIntN(gapStart, index);
	gapStart += 2;
      }
  }

  /** Write/set the document-uri property of the current document.
   * Only allowed immediately following startDocument.   */
  public void writeDocumentUri (Object uri)
  {
    ensureSpace(3);
    int index = find(uri);
    data[gapStart++] = DOCUMENT_URI;
    setIntN(gapStart, index);
    gapStart += 2;
  }

  public void writeComment(char[] chars, int offset, int length)
  {
    ensureSpace(3+length);
    int i = gapStart;
    data[i++] = COMMENT;
    setIntN(i, length);
    i += 2;
    System.arraycopy(chars, offset, data, i, length);
    gapStart = i + length;
  }

  public void writeComment(String comment, int offset, int length)
  {
    ensureSpace(3+length);
    int i = gapStart;
    data[i++] = COMMENT;
    setIntN(i, length);
    i += 2;
    comment.getChars(offset, offset+length, data, i);
    gapStart = i + length;
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    ensureSpace(5+length);
    int i = gapStart;
    data[i++] = PROCESSING_INSTRUCTION;
    int index = find(target);
    setIntN(i, index);
    setIntN(i+2, length);
    i += 4;
    System.arraycopy(content, offset, data, i, length);
    gapStart = i + length;
  }

  public void writeProcessingInstruction(String target, String content,
					 int offset, int length)
  {
    ensureSpace(5+length);
    int i = gapStart;
    data[i++] = PROCESSING_INSTRUCTION;
    int index = find(target);
    setIntN(i, index);
    setIntN(i+2, length);
    i += 4;
    content.getChars(offset, offset+length, data, i);
    gapStart = i + length;
  }

  public void startElement (Object type)
  {
    startElement(find(type));
  }

  public void startDocument ()
  {
    ensureSpace(5+1);
    gapEnd--;
    int p = gapStart;
    data[p] = BEGIN_DOCUMENT;
    if (docStart != 0)
      throw new Error("nested document");
    docStart = p+1;
    setIntN(p+1, gapEnd - data.length);
    setIntN(p+3, currentParent == -1 ? -1 : currentParent-p);  // parent_offset
    currentParent = p;
    gapStart = p + 5;
    currentParent = p;
    data[gapEnd] = END_DOCUMENT;
  }

  public void endDocument()
  {
    if (data[gapEnd] != END_DOCUMENT || docStart <= 0
        || data[currentParent] != BEGIN_DOCUMENT)
      throw new Error("unexpected endDocument");
    // Move the END_DOCUMENT to before the gap.
    gapEnd++;
    setIntN(docStart, gapStart - docStart + 1);
    docStart = 0;
    data[gapStart++] = END_DOCUMENT;
    int parent = getIntN(currentParent+3);
    currentParent = parent >= -1 ? parent : currentParent + parent;
  }

  public void beginEntity (Object base)
  {
    // Ideally, we want to ignore a beginEnity if and only if it is redundant.
    // There are also problems in the current implementation with
    // nested (or maybe any non-top-level?) BEGIN_ENTITY nodes.
    // So for now, let's keep it simple.
    if (gapStart != 0)
      return;
    ensureSpace(BEGIN_ENTITY_SIZE+1);
    gapEnd--;
    int p = gapStart;
    data[p] = BEGIN_ENTITY;
    setIntN(p+1, find(base));
    setIntN(p+3, currentParent == -1 ? -1 : currentParent-p);  // parent_offset
    gapStart = p + 5;
    currentParent = p;
    data[gapEnd] = END_ENTITY;
  }

  public void endEntity ()
  {
    // See comment in beginEntity.
    if (gapEnd + 1 != data.length || data[gapEnd] != END_ENTITY)
      return;
    if (/*data[gapEnd] != END_ENTITY || */
        data[currentParent] != BEGIN_ENTITY)
      throw new Error("unexpected endEntity");
    // Move the END_ENTITY to before the gap.
    gapEnd++;
    data[gapStart++] = END_ENTITY;
    int parent = getIntN(currentParent+3);
    currentParent = parent >= -1 ? parent : currentParent + parent;
  }

  public void startElement(int index)
  {
    ensureSpace(3 + 7);
    gapEnd -= 7;
    data[gapStart++] = BEGIN_ELEMENT_LONG;
    setIntN(gapStart, gapEnd - data.length); // end_offset
    gapStart += 2;
    data[gapEnd] = END_ELEMENT_LONG;
    setIntN(gapEnd + 1, index);  // begin_offset
    setIntN(gapEnd + 3, gapStart - 3);  // begin_offset
    setIntN(gapEnd + 5, currentParent);  // parent_offset
    currentParent = gapStart - 3;
  }

  public void setElementName (int elementIndex, int nameIndex)
  {
    if (data[elementIndex] == BEGIN_ELEMENT_LONG)
      {
        int j = getIntN(elementIndex+1);
        elementIndex = j + (j < 0 ? data.length : elementIndex);
      }
    if (elementIndex < gapEnd)
      throw new Error("setElementName before gapEnd");
    setIntN(elementIndex + 1, nameIndex);
  }

  public void endElement ()
  {
    if (data[gapEnd] != END_ELEMENT_LONG)
      throw new Error("unexpected endElement");
    int index = getIntN(gapEnd + 1);
    int begin = getIntN(gapEnd + 3);
    int parent = getIntN(gapEnd + 5);
    currentParent = parent;
    gapEnd += 7;
    int offset = gapStart - begin;
    int parentOffset = begin - parent;
    if (index < BEGIN_ELEMENT_SHORT_INDEX_MAX
	&& offset < 0x10000 && parentOffset < 0x10000)
      {
	data[begin] = (char) (BEGIN_ELEMENT_SHORT | index);
	data[begin + 1] = (char) offset;  // end_offset
	data[begin + 2] = (char) parentOffset;
	data[gapStart] = END_ELEMENT_SHORT;
	data[gapStart + 1] = (char) offset; // begin_offset
	gapStart += 2;
      }
    else
      {
	data[begin] = BEGIN_ELEMENT_LONG;
	setIntN(begin + 1, offset);
	data[gapStart] = END_ELEMENT_LONG;
	setIntN(gapStart + 1, index);
	setIntN(gapStart + 3, - offset);
	if (parent >= gapStart || begin <= gapStart)
	  parent -= gapStart;
	setIntN(gapStart + 5, parent);
	gapStart += 7;
      }
  }

  public void startAttribute (Object attrType)
  {
    startAttribute(find(attrType));
  }

  public void startAttribute (int index)
  {
    /* This needs to be tested.  FIXME.  Anyway only solves limited problem.
    // If there is whitespace and nothing else between the BEGIN_ELEMENT_LONG
    // and the current position, get rid of the spaces.
    int i = currentParent;
    if (i > 0 && (i += 3) < gapStart)
      {
	for (int j = i;  ; j++)
	  {
	    if (j == gapStart)
	      {
		gapStart = i;
		break;
	      }
	    char c = data[j];
	    if (c != ' ' && c != '\t' && c != '\n' && c != '\r')
	      break;
	  }
      }
    */

    ensureSpace(5 + 1);
    gapEnd--;
    data[gapStart++] = BEGIN_ATTRIBUTE_LONG;
    if (attrStart != 0)
      throw new Error("nested attribute");
    attrStart = gapStart;
    setIntN(gapStart, index);
    setIntN(gapStart + 2, gapEnd - data.length);
    gapStart += 4;
    data[gapEnd] = END_ATTRIBUTE;
  }

  public void setAttributeName (int attrIndex, int nameIndex)
  {
    setIntN(attrIndex + 1, nameIndex);
  }

  public void endAttribute()
  {
    if (attrStart <= 0)
      return;
    if (data[gapEnd] != END_ATTRIBUTE)
      throw new Error("unexpected endAttribute");
    // Move the END_ATTRIBUTES to before the gap.
    gapEnd++;
    setIntN(attrStart+2, gapStart - attrStart + 1);
    attrStart = 0;
    data[gapStart++] = END_ATTRIBUTE;
  }

  public Consumer append (char c)
  {
    write(c);
    return this;
  }

  public void write (int c)
  {
    ensureSpace(3);
    if (c <= MAX_CHAR_SHORT)
      data[gapStart++] = (char) c;
    else if (c < 0x10000)
      {
	data[gapStart++] = CHAR_FOLLOWS;
	data[gapStart++] = (char) c;
      }
    else
      Char.print(c, this);
  }

  public void writeBoolean(boolean v)
  {
    ensureSpace(1);
    data[gapStart++] = v ? BOOL_TRUE : BOOL_FALSE;
  }

  public void writeByte(int v)
  {
    ensureSpace(1);
    data[gapStart++] = (char) (BYTE_PREFIX + (v & 0xFF));
  }

  public void writeInt(int v)
  {
    ensureSpace(3);
    if (v >= MIN_INT_SHORT && v <= MAX_INT_SHORT)
      data[gapStart++] = (char) (INT_SHORT_ZERO + v);
    else
      {
	data[gapStart++] = INT_FOLLOWS;
	setIntN(gapStart, v);
	gapStart += 2;
      }
  }

  public void writeLong(long v)
  {
    ensureSpace(5);
    data[gapStart++] = LONG_FOLLOWS;
    data[gapStart++] = (char) (v >>> 48);
    data[gapStart++] = (char) (v >>> 32);
    data[gapStart++] = (char) (v >>> 16);
    data[gapStart++] = (char) v;
  }

  public void writeFloat(float v)
  {
    ensureSpace(3);
    int i = Float.floatToIntBits(v);
    data[gapStart++] = FLOAT_FOLLOWS;
    data[gapStart++] = (char) (i >>> 16);
    data[gapStart++] = (char) i;
  }

  public void writeDouble(double v)
  {
    ensureSpace(5);
    long l = Double.doubleToLongBits(v);
    data[gapStart++] = DOUBLE_FOLLOWS;
    data[gapStart++] = (char) (l >>> 48);
    data[gapStart++] = (char) (l >>> 32);
    data[gapStart++] = (char) (l >>> 16);
    data[gapStart++] = (char) l;
  }

  public boolean ignoring()
  {
    return false;
  }

  public void writeJoiner ()
  {
    ensureSpace(1);
    data[gapStart++] = JOINER;
  }

  public void write(char[] buf, int off, int len)
  {
    if (len == 0)
      writeJoiner();
    ensureSpace(len);
    while (len > 0)
      {
	char ch = buf[off++];
	len--;
	if (ch <= MAX_CHAR_SHORT)
	  data[gapStart++] = ch;
	else
	  {
	    write(ch);
	    ensureSpace(len);
	  }
      }
  }

  public void write (String str)
  {
    write(str, 0, str.length());
  }

  /* #ifdef use:java.lang.CharSequence */
  public void write (CharSequence str, int start, int length)
  /* #else */
  // public void write (String str, int start, int length)
  /* #endif */
  {
    if (length == 0)
      writeJoiner();
    ensureSpace(length);
    while (length > 0)
      {
	char ch = str.charAt(start++);
	length--;
	if (ch <= MAX_CHAR_SHORT)
	  data[gapStart++] = ch;
	else
	  {
	    write(ch);
	    ensureSpace(length);
	  }
      }
  }

  public void writeCDATA (char[] chars, int offset, int length)
  {
    ensureSpace(3+length);
    int i = gapStart;
    data[i++] = CDATA_SECTION;
    setIntN(i, length);
    i += 2;
    System.arraycopy(chars, offset, data, i, length);
    gapStart = i + length;
  }

  /* #ifdef use:java.lang.CharSequence */
  public Consumer append (CharSequence csq)
  {
    if (csq == null)
      csq = "null";
    return append(csq, 0, csq.length());
  }

  public Consumer append (CharSequence csq, int start, int end)
  {
    if (csq == null)
      csq = "null";
    for (int i = start; i < end;  i++)
      append(csq.charAt(i));
    return this;
  }
  /* #else */
  // public Consumer append (String str)
  // {
  //   if (str == null)
  //     str = "null";
  //    int len = str.length();
  //    for (int i = 0; i < len;  i++)
  //      append(str.charAt(i));
  //    return this;
  // }
  /* #endif */

  public boolean isEmpty()
  {
    // FIXME does not work if we allow comment() entries!
    int pos = gapStart == 0 ? gapEnd : 0;
    return pos == data.length;
  }

  public int size()
  {
    int size = 0;
    int i = 0;
    for (;;)
      {
	i = nextPos(i);
	if (i == 0)
	  return size;
	size++;
      }
  }

  public int createPos(int index, boolean isAfter)
  {
    return createRelativePos(0, index, isAfter);
  }

  public final int posToDataIndex (int ipos)
  {
    if (ipos == -1)
      return data.length;
    int index = ipos >>> 1;
    if ((ipos & 1) != 0)
      index--;
    if (index == gapStart)
      index += gapEnd - gapStart;
    if ((ipos & 1) != 0)
      {
	index = nextDataIndex(index);
	if (index < 0)
	  return data.length;
	if (index == gapStart)
	  index += gapEnd - gapStart;
      }
    return index;
  }

  public int firstChildPos (int ipos)
  {
    int index = gotoChildrenStart(posToDataIndex(ipos));
    if (index < 0)
      return 0;
    return index << 1;
  }

  public final int gotoChildrenStart(int index)
  {
    if (index == data.length)
      return -1;
    char datum = data[index];
    if ((datum >= BEGIN_ELEMENT_SHORT
	 && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
	|| datum == BEGIN_ELEMENT_LONG)
      index += 3;
    else if (datum == BEGIN_DOCUMENT || datum == BEGIN_ENTITY)
      index += 5;
    else
      return -1;
    for (;;)
      {
	if (index >= gapStart)
	  index += gapEnd - gapStart;
	datum = data[index];
	if (datum == BEGIN_ATTRIBUTE_LONG)
	  {
	    int end = getIntN(index+3);
	    index = end + (end < 0 ? data.length : index);
	  }
	else if (datum == END_ATTRIBUTE || datum == JOINER)
	  index++;
	else if (datum == DOCUMENT_URI)
	  index += 3;
	else
	  break;
      }
    return index;
  }

  public int parentPos (int ipos)
  {
    int index = posToDataIndex(ipos);
    for (;;)
      {
        index = parentOrEntityI(index);
        if (index == -1)
          return -1;
        if (data[index] != BEGIN_ENTITY)
          return index << 1;
      }
  }

  public int parentOrEntityPos (int ipos)
  {
    int index = parentOrEntityI(posToDataIndex(ipos));
    return index < 0 ? -1 : index << 1;
  }

  public int parentOrEntityI (int index)
  {
    if (index == data.length)
      return -1;
    char datum = data[index];
    if (datum == BEGIN_DOCUMENT || datum == BEGIN_ENTITY)
      {
	int parent_offset = getIntN(index+3);
        if (parent_offset >= -1)
          return parent_offset;
        else
          return index + parent_offset;
      }
    if (datum >= BEGIN_ELEMENT_SHORT
	 && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
      {
	int parent_offset = data[index+2];
	return parent_offset == 0 ? -1 : index - parent_offset;
      }
    if (datum == BEGIN_ELEMENT_LONG)
      {
	int end_offset = getIntN(index+1);
	end_offset += end_offset < 0 ? data.length : index;
	int parent_offset = getIntN(end_offset+5);
	if (parent_offset == 0)
	  return -1;
	if (parent_offset < 0)
	  parent_offset += end_offset;
	return parent_offset;
      }
    for (;;)
      {
	if (index == gapStart)
	  index = gapEnd;
	if (index == data.length)
	  return -1;
	datum = data[index];
	switch (datum)
	  {
	  case END_ELEMENT_SHORT:
	    return index - data[index+1];
	  case END_ELEMENT_LONG:
	    int begin_offset = getIntN(index+3);
	    if (begin_offset < 0)
	      begin_offset += index;
	    return begin_offset;
	  case END_ATTRIBUTE:
	    index++;
	    continue;
	  case END_DOCUMENT:
	    return -1;
	  default:
	    index = nextDataIndex(index);
	  }
	if (index < 0)
	  break;
      }
    return -1;
  }

  public int getAttributeCount (int parent)
  {
    int n = 0;
    for (int attr = firstAttributePos(parent);
         attr != 0 && getNextKind(attr) == Sequence.ATTRIBUTE_VALUE;
         attr = nextPos(attr))
      n++;
    return n;
  }

  public boolean gotoAttributesStart(TreePosition pos)
  {
    int index = gotoAttributesStart(pos.ipos >> 1);
    if (index < 0)
      return false;
    pos.push(this, index << 1);
    return true;
  }

  public int firstAttributePos (int ipos)
  {
    int index = gotoAttributesStart(posToDataIndex(ipos));
    return index < 0 ? 0 : index << 1;
  }

  public int gotoAttributesStart(int index)
  {
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return -1;
    char datum = data[index];
    if ((datum >= BEGIN_ELEMENT_SHORT
	 && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
	|| datum == BEGIN_ELEMENT_LONG)
      return index + 3;
    else
      return -1;
  }

  public Object get (int index)
  {
    int i = 0;
    while (--index >= 0)
      {
	i = nextPos(i);
	if (i == 0)
	  throw new IndexOutOfBoundsException();
      }
    return getPosNext(i);
  }

  public boolean consumeNext(int ipos, Consumer out)
  {
    if (! hasNext(ipos))
      return false;
    int start = posToDataIndex(ipos);
    int end = nextNodeIndex(start, -1 >>> 1);
    if (end == start)
      end = nextDataIndex(start);
    if (end >= 0)
      consumeIRange(start, end, out);
    return true;
  }

  public void consumePosRange(int startPos, int endPos, Consumer out)
  {
    consumeIRange(posToDataIndex(startPos), posToDataIndex(endPos), out);
  }

  public int consumeIRange(int startPosition, int endPosition, Consumer out)
  {
    int pos = startPosition;
    int limit = startPosition <= gapStart && endPosition > gapStart ? gapStart
      : endPosition;
    int index;
    for (;;)
      {
	if (pos >= limit)
	  {
	    if (pos == gapStart && endPosition > gapEnd)
	      {
		pos = gapEnd;
		limit = endPosition;
	      }
	    else
	      break;
	  }

	char datum = data[pos++];

	if (datum <= MAX_CHAR_SHORT)
	  {
	    int start = pos - 1;
	    int lim = limit;
	    for (;;)
	      {
		if (pos >= lim)
		  break;
		datum = data[pos++];
		if (datum > MAX_CHAR_SHORT)
		  {
		    pos--;
		    break;
		  }
	      }
	    out.write(data, start, pos - start);
	    continue;
	  }
	if (datum >= OBJECT_REF_SHORT
	     && datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
	  {
	    out.writeObject(objects[datum-OBJECT_REF_SHORT]);
	    continue;
	  }
	if (datum >= BEGIN_ELEMENT_SHORT
	    && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
	  {
	    index = datum-BEGIN_ELEMENT_SHORT;
	    out.startElement(objects[index]);
	    pos += 2;
	    continue;
	  }
	/*
	if ((datum & 0xFF00) == BYTE_PREFIX)
	  {
	    out.writeByte((byte) datum);
	    continue;
	  }
	*/
	if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	    && datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
	  {
	    out.writeInt(datum - INT_SHORT_ZERO);
	    continue;
	  }
	switch (datum)
	  {
	  case BEGIN_DOCUMENT:
	    out.startDocument();
	    pos += 4;
	    continue;
	  case END_DOCUMENT:
	    out.endDocument();
	    continue;
          case BEGIN_ENTITY:
	    if (out instanceof TreeList)
	      ((TreeList) out).beginEntity(objects[getIntN(pos)]);
	    pos += 4;
            continue;
          case END_ENTITY:
	    if (out instanceof TreeList)
	      ((TreeList) out).endEntity();
             continue;
	  case DOCUMENT_URI:
	    if (out instanceof TreeList)
	      ((TreeList) out).writeDocumentUri(objects[getIntN(pos)]);
	    pos += 2;
	    continue;
	  case COMMENT:
	    {
	      int length = getIntN(pos);
	      pos += 2;
	      if (out instanceof XConsumer)
		((XConsumer) out).writeComment(data, pos, length);
	      pos += length;
	    }
	    continue;
	  case CDATA_SECTION:
	    {
	      int length = getIntN(pos);
	      pos += 2;
	      if (out instanceof XConsumer)
		((XConsumer) out).writeCDATA(data, pos, length);
              else
                out.write(data, pos, length);
	      pos += length;
	    }
	    continue;
	  case PROCESSING_INSTRUCTION:
	    {
	      String target = (String) objects[getIntN(pos)];
	      int length = getIntN(pos+2);
	      pos += 4;
	      if (out instanceof XConsumer)
		((XConsumer) out).writeProcessingInstruction(target, data,
							     pos, length);
	      pos += length;
	    }
	    continue;
	  case BOOL_FALSE:
	  case BOOL_TRUE:
	    out.writeBoolean(datum != BOOL_FALSE);
	    continue;
          case JOINER:
            out.write("");
            continue;
	  case CHAR_FOLLOWS:
	    out.write(data, pos, 1 + datum - CHAR_FOLLOWS);
	    pos++;
	    continue;
	  case POSITION_PAIR_FOLLOWS:
	    {
	      AbstractSequence seq = (AbstractSequence) objects[getIntN(pos)];
	      int ipos = getIntN(pos+2);
	      if (out instanceof PositionConsumer)
		((PositionConsumer) out).writePosition(seq, ipos);
	      else
		out.writeObject(seq.getIteratorAtPos(ipos));
	      pos += 4;
	    }
	    continue;
	  case POSITION_REF_FOLLOWS:
	    if (out instanceof PositionConsumer)
	      {
		((PositionConsumer) out).writePosition((SeqPosition) objects[getIntN(pos)]);
		pos += 2;
		continue;
	      }
	    // ... else fall through ...
	  case OBJECT_REF_FOLLOWS:
	    out.writeObject(objects[getIntN(pos)]);
	    pos += 2;
	    continue;
	  case END_ELEMENT_SHORT:
	    pos++;
	    out.endElement();
	    continue;
	  case BEGIN_ELEMENT_LONG:
	    index = getIntN(pos);
	    index += index >= 0 ? pos - 1 : data.length;
	    pos += 2;
	    index = getIntN(index + 1);
	    out.startElement(objects[index]);
	    continue;
	  case END_ELEMENT_LONG:
	    index = getIntN(pos);
	    out.endElement();
	    pos += 6;
	    continue;
	  case BEGIN_ATTRIBUTE_LONG:
	    index = getIntN(pos);
	    out.startAttribute(objects[index]);
	    pos += 4;
	    continue;
	  case END_ATTRIBUTE:
	    out.endAttribute();
	    continue;
	  case INT_FOLLOWS:
	    out.writeInt(getIntN(pos));
	    pos += 2;
	    continue;
	  case FLOAT_FOLLOWS:
	    out.writeFloat(Float.intBitsToFloat(getIntN(pos)));
	    pos += 2;
	    continue;
	  case LONG_FOLLOWS:
	    out.writeLong(getLongN(pos));
	    pos += 4;
	    continue;
	  case DOUBLE_FOLLOWS:
	    out.writeDouble(Double.longBitsToDouble(getLongN(pos)));
	    pos += 4;
	    continue;
	  default:
	    throw new Error("unknown code:"+(int) datum);
	  }
      }
    return pos;
  }

  public void toString (String sep, StringBuffer sbuf)
  {
    int pos = 0;
    int limit = gapStart;
    int index;
    boolean seen = false;
    boolean inStartTag = false;
    boolean inAttribute = false;
    for (;;)
      {
	if (pos >= limit)
	  {
	    if (pos == gapStart)
	      {
		pos = gapEnd;
		limit = data.length;
		if (pos == limit)
		  break;
	      }
	    else
	      break;
	  }

	char datum = data[pos++];

	if (datum <= MAX_CHAR_SHORT)
	  {
	    int start = pos - 1;
	    int lim = limit;
	    for (;;)
	      {
		if (pos >= lim)
		  break;
		datum = data[pos++];
		if (datum > MAX_CHAR_SHORT)
		  {
		    pos--;
		    break;
		  }
	      }
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    sbuf.append(data, start, pos - start);
	    seen = false;
	    continue;
	  }
	if (datum >= OBJECT_REF_SHORT
	     && datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
	  {
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(objects[datum-OBJECT_REF_SHORT]);
	    continue;
	  }
	if (datum >= BEGIN_ELEMENT_SHORT
	    && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
	  {
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    index = datum-BEGIN_ELEMENT_SHORT;
	    if (seen) sbuf.append(sep);
	    sbuf.append('<');
	    sbuf.append(objects[index].toString());
	    pos += 2;
	    seen = false;
	    inStartTag = true;
	    continue;
	  }
	if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	    && datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
	  {
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(datum - INT_SHORT_ZERO);
	    continue;
	  }
	switch (datum)
	  {
	  case BEGIN_DOCUMENT:
	  case BEGIN_ENTITY:
	    pos += 4;
	    continue;
	  case DOCUMENT_URI:
	    pos += 2;
	    continue;
	  case COMMENT:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    index = getIntN(pos); // comment length
	    pos += 2;
	    sbuf.append("<!--");
	    sbuf.append(data, pos, index);
	    sbuf.append("-->");
	    pos += index;
	    continue;
	  case CDATA_SECTION:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    index = getIntN(pos); // comment length
	    pos += 2;
	    sbuf.append("<![CDATA[");
	    sbuf.append(data, pos, index);
	    sbuf.append("]]>");
	    pos += index;
	    continue;
	  case PROCESSING_INSTRUCTION:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    sbuf.append("<?");
	    index = getIntN(pos); // target
	    pos += 2;
	    sbuf.append(objects[index]);
	    index = getIntN(pos); // comment length
	    pos += 2;
	    if (index > 0)
	      {
		sbuf.append(' ');
		sbuf.append(data, pos, index);
		pos += index;
	      }
	    sbuf.append("?>");
	    continue;
	  case END_DOCUMENT:
	  case END_ENTITY:
	    continue;
	  case BOOL_FALSE:
	  case BOOL_TRUE:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(datum != BOOL_FALSE);
	    continue;
          case JOINER:
            continue;
	  case CHAR_FOLLOWS:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    sbuf.append(data, pos, 1 + datum - CHAR_FOLLOWS);
	    seen = false;
	    pos++;
	    continue;
	  case POSITION_PAIR_FOLLOWS:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    {
	      AbstractSequence seq = (AbstractSequence) objects[getIntN(pos)];
	      int ipos = getIntN(pos+2);
	      // This could lead to to a lot of copying.  FIXME.
	      sbuf.append(seq.getIteratorAtPos(ipos));
	      pos += 4;
	    }
	    continue;
	  case POSITION_REF_FOLLOWS:
	  case OBJECT_REF_FOLLOWS:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(objects[getIntN(pos)]);
	    pos += 2;
	    continue;
	  case BEGIN_ELEMENT_LONG:
	    index = getIntN(pos);
	    index += index >= 0 ? pos - 1 : data.length;
	    pos += 2;
	    index = getIntN(index + 1);
	    if (inStartTag) sbuf.append('>');
	    else if (seen) sbuf.append(sep);
	    sbuf.append('<');
	    sbuf.append(objects[index]);
	    seen = false;
	    inStartTag = true;
	    continue;
	  case END_ELEMENT_LONG:
	  case END_ELEMENT_SHORT:
	    if (datum == END_ELEMENT_SHORT)
	      {
		index = data[pos++];
		index = data[pos - 2 - index] - BEGIN_ELEMENT_SHORT;
	      }
	    else
	      {
		index = getIntN(pos);
		pos += 6;
	      }
	    if (inStartTag)
	      sbuf.append("/>");
	    else
	      {
		sbuf.append("</");
		sbuf.append(objects[index]);
		sbuf.append('>');
	      }
	    inStartTag = false;
	    seen = true;
	    continue;
	  case BEGIN_ATTRIBUTE_LONG:
	    index = getIntN(pos);
	    sbuf.append(' ');
	    sbuf.append(objects[index]);
	    sbuf.append("=\"");
	    inAttribute = true;
	    inStartTag = false;
	    pos += 4;
	    continue;
	  case END_ATTRIBUTE:
	    sbuf.append('"');
	    inAttribute = false;
	    inStartTag = true;
	    seen = false;
	    continue;
	  case INT_FOLLOWS:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(getIntN(pos));
	    pos += 2;
	    continue;
	  case FLOAT_FOLLOWS:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(Float.intBitsToFloat(getIntN(pos)));
	    pos += 2;
	    continue;
	  case LONG_FOLLOWS:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(getLongN(pos));
	    pos += 4;
	    continue;
	  case DOUBLE_FOLLOWS:
	    if (inStartTag) { sbuf.append('>'); inStartTag = false; }
	    if (seen) sbuf.append(sep); else seen = true;
	    sbuf.append(Double.longBitsToDouble(getLongN(pos)));
	    pos += 4;
	    continue;
	  default:
	    throw new Error("unknown code:"+(int) datum);
	  }
      }
  }

  public boolean hasNext(int ipos)
  {
    int index = posToDataIndex(ipos);
    if (index == data.length)
      return false;
    char ch = data[index];
    return ch != END_ATTRIBUTE && ch != END_ELEMENT_SHORT
      && ch != END_ELEMENT_LONG && ch != END_DOCUMENT;
  }

  public int getNextKind(int ipos)
  {
    return getNextKindI(posToDataIndex(ipos));
  }

  public int getNextKindI (int index)
  {
    if (index == data.length)
      return Sequence.EOF_VALUE;
    char datum = data[index];
    if (datum <= MAX_CHAR_SHORT)
      return Sequence.CHAR_VALUE;
    if (datum >= OBJECT_REF_SHORT
	&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
      return Sequence.OBJECT_VALUE;
    if (datum >= BEGIN_ELEMENT_SHORT
	    && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
      return Sequence.ELEMENT_VALUE;
    if ((datum & 0xFF00) == BYTE_PREFIX)
      return Sequence.TEXT_BYTE_VALUE;
    if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
      return Sequence.INT_S32_VALUE;
    switch (datum)
      {
      case BOOL_FALSE:
      case BOOL_TRUE:
	return Sequence.BOOLEAN_VALUE;
      case INT_FOLLOWS:
	return Sequence.INT_S32_VALUE;
      case LONG_FOLLOWS:
	return Sequence.INT_S64_VALUE;
      case FLOAT_FOLLOWS:
	return Sequence.FLOAT_VALUE;
      case DOUBLE_FOLLOWS:
	return Sequence.DOUBLE_VALUE;
      case CHAR_FOLLOWS:
      case BEGIN_DOCUMENT:
	return Sequence.DOCUMENT_VALUE;
      case BEGIN_ENTITY:
        return getNextKind((index+BEGIN_ENTITY_SIZE) << 1);
      case BEGIN_ELEMENT_LONG:
	return Sequence.ELEMENT_VALUE;
      case END_ELEMENT_SHORT:
      case END_ELEMENT_LONG:
      case END_ATTRIBUTE:
      case END_DOCUMENT:
      case END_ENTITY:
	return Sequence.EOF_VALUE;
      case BEGIN_ATTRIBUTE_LONG:
	return Sequence.ATTRIBUTE_VALUE;
      case CDATA_SECTION:
	return Sequence.CDATA_VALUE;
      case COMMENT:
	return Sequence.COMMENT_VALUE;
      case PROCESSING_INSTRUCTION:
	return Sequence.PROCESSING_INSTRUCTION_VALUE;
      case DOCUMENT_URI:  // FIXME
      case POSITION_REF_FOLLOWS: // FIXME	
      case POSITION_PAIR_FOLLOWS:
      case OBJECT_REF_FOLLOWS:
      case JOINER: // FIXME
      default:
	return Sequence.OBJECT_VALUE;
      }

  }

  public Object getNextTypeObject (int ipos)
  {
    int index = posToDataIndex(ipos);
    char datum;
    for (;;)
      {
        if (index == data.length)
          return null;
        datum = data[index];
        if (datum != BEGIN_ENTITY)
          break;
        index += BEGIN_ENTITY_SIZE;
      }
    if (datum >= BEGIN_ELEMENT_SHORT
	&& datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
      index = datum-BEGIN_ELEMENT_SHORT;
    else if (datum == BEGIN_ELEMENT_LONG)
      {
	int j = getIntN(index+1);
	j += j < 0 ? data.length : index;
	index = getIntN(j + 1);
      }
    else if (datum == BEGIN_ATTRIBUTE_LONG)
      index = getIntN(index + 1);
    else if (datum == PROCESSING_INSTRUCTION)
      index = getIntN(index + 1);
    else
      return null;
    return index < 0 ? null : objects[index];
  }

  public Object getPosPrevious(int ipos)
  {
    if ((ipos & 1) != 0 && ipos != -1)
      return getPosNext(ipos - 3);
    else
      return super.getPosPrevious(ipos);
  }

  private Object copyToList(int startPosition, int endPosition)
  {
    return new TreeList(this, startPosition, endPosition);
  }

  /** Return following value (like getPosNext), as an integer. */
  public int getPosNextInt (int ipos)
  {
    int index = posToDataIndex(ipos);
    if (index < data.length)
      {
	char datum = data[index];
	if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	    && datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
	  return datum-INT_SHORT_ZERO;
	if (datum == INT_FOLLOWS)
	  return getIntN(index+1);
      }
    return ((Number) getPosNext(ipos)).intValue();
  }

  public Object getPosNext(int ipos)
  {
    int index = posToDataIndex(ipos);
    if (index == data.length)
      return Sequence.eofValue;
    char datum = data[index];
    if (datum <= MAX_CHAR_SHORT)
      return Convert.toObject(datum);
    if (datum >= OBJECT_REF_SHORT
	&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
      return objects[datum-OBJECT_REF_SHORT];
    if (datum >= BEGIN_ELEMENT_SHORT
	    && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
      return copyToList(index, index + data[index+1] + 2);
    /*
    if ((datum & 0xFF00) == BYTE_PREFIX)
      return Sequence.TEXT_BYTE_VALUE;
    */
    if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
      return Convert.toObject(datum-INT_SHORT_ZERO);
    switch (datum)
      {
      case BEGIN_DOCUMENT:
	{
	  int end_offset = getIntN(index+1);
	  end_offset += end_offset < 0 ? data.length : index;
	  end_offset++;
	  /* Need to be careful about this.
	  if (index == 0
	      && (end_offset == data.length
		  || (end_offset == gapStart && gapEnd == data.length)))
	    return this;
	  */
	  return copyToList(index, end_offset);
	}
      case BOOL_FALSE:
      case BOOL_TRUE:
	return Convert.toObject(datum != BOOL_FALSE);
      case INT_FOLLOWS:
	return Convert.toObject(getIntN(index+1));
      case LONG_FOLLOWS:
	return Convert.toObject(getLongN(index+1));
      case FLOAT_FOLLOWS:
	return Convert.toObject(Float.intBitsToFloat(getIntN(index+1)));
      case DOUBLE_FOLLOWS:
	return Convert.toObject(Double.longBitsToDouble(getLongN(index+1)));
      case CHAR_FOLLOWS:
	return Convert.toObject(data[index+1]);
      case BEGIN_ATTRIBUTE_LONG:
	{
	  int end_offset = getIntN(index+3);
	  end_offset += end_offset < 0 ? data.length : index;
	  return copyToList(index, end_offset+1);
	}
      case BEGIN_ELEMENT_LONG:
	{
	  int end_offset = getIntN(index+1);
	  end_offset += end_offset < 0 ? data.length : index;
	  return copyToList(index, end_offset+7);
	}
      case END_ELEMENT_SHORT:
      case END_ELEMENT_LONG:
      case END_ATTRIBUTE:
      case END_DOCUMENT:
	return Sequence.eofValue;
      case POSITION_REF_FOLLOWS:
      case OBJECT_REF_FOLLOWS:
	return objects[getIntN(index+1)];
      case JOINER:
        return "";
      case POSITION_PAIR_FOLLOWS: //FIXME
	AbstractSequence seq = (AbstractSequence) objects[getIntN(index+1)];
	ipos = getIntN(index+3);
	return seq.getIteratorAtPos(ipos);
      default:
	throw unsupported("getPosNext, code="+Integer.toHexString(datum));
      }
  }

  public void stringValue (int startIndex, int endIndex, StringBuffer sbuf)
  {
    int index = startIndex;
    while (index < endIndex && index >= 0)
      index = stringValue(false, index, sbuf);
  }

  public int stringValue(int index, StringBuffer sbuf)
  {
    int next = nextNodeIndex(index, -1 >>> 1);
    if (next > index)
      {
        stringValue(index, next, sbuf);
	return index;
      }
    else
      return stringValue(false, index, sbuf);
  }

  public int stringValue(boolean inElement, int index, StringBuffer sbuf)
  {
    Object value = null;
    int doChildren = 0, j;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return -1;
    char datum = data[index];
    index++;
    boolean spaceNeeded = false;
    if (datum <= MAX_CHAR_SHORT)
      {
	sbuf.append(datum);
	return index;
      }
    if (datum >= OBJECT_REF_SHORT
	&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
      {
        if (spaceNeeded)
          sbuf.append(' ');
	value = objects[datum-OBJECT_REF_SHORT];
        spaceNeeded = false;
      }
    else if (datum >= BEGIN_ELEMENT_SHORT
	     && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
      {
	doChildren = index + 2;
	index = data[index] + index + 1;
      }
    else if ((datum & 0xFF00) == BYTE_PREFIX)
      {
	sbuf.append(datum & 0xFF);
	return index;
      }
    else if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
      {
	sbuf.append((int) datum - INT_SHORT_ZERO);
	return index;
      }
    else
      {
	switch (datum)
	  {
	  case DOCUMENT_URI:
	    return index + 2;
	  case PROCESSING_INSTRUCTION:
	    index += 2;
	    /* ... fall through ... */
	  case CDATA_SECTION:
	  case COMMENT:
	    {
	      int length = getIntN(index);
	      index += 2;
	      if (! inElement || datum == CDATA_SECTION)
		sbuf.append(data, index, length);
	      return index + length;
	    }
	  case BOOL_FALSE:
	  case BOOL_TRUE:
            if (spaceNeeded)
              sbuf.append(' ');
	    sbuf.append(datum != BOOL_FALSE);
            spaceNeeded = true;
	    return index;
	  case INT_FOLLOWS:
            if (spaceNeeded)
              sbuf.append(' ');
	    sbuf.append(getIntN(index));
            spaceNeeded = true;
	    return index + 2;
	  case LONG_FOLLOWS:
            if (spaceNeeded)
              sbuf.append(' ');
	    sbuf.append(getLongN(index));
            spaceNeeded = true;
	    return index + 4;
	  case FLOAT_FOLLOWS:
            if (spaceNeeded)
              sbuf.append(' ');
	    sbuf.append(Float.intBitsToFloat(getIntN(index)));
            spaceNeeded = true;
	    return index + 2;
	  case DOUBLE_FOLLOWS:
            if (spaceNeeded)
              sbuf.append(' ');
	    sbuf.append(Double.longBitsToDouble(getLongN(index)));
            spaceNeeded = true;
	    return index + 4;
	  case CHAR_FOLLOWS:
            spaceNeeded = false;
	    sbuf.append(data[index]);
	    return index + 1;
	  case BEGIN_DOCUMENT:
	  case BEGIN_ENTITY:
	    doChildren = index + 4;
	    index = nextDataIndex(index-1);
	    break;
	  case BEGIN_ELEMENT_LONG:	
            spaceNeeded = false;
	    doChildren = index + 2;
	    j = getIntN(index);
	    j += j < 0 ? data.length : index-1;
	    index = j + 7;
	    break;
          case JOINER:
            spaceNeeded = false;
            break;
	  case END_ELEMENT_SHORT:
	  case END_ELEMENT_LONG:
	  case END_ATTRIBUTE:
	  case END_DOCUMENT:
	  case END_ENTITY:
	    return -1;
	  case BEGIN_ATTRIBUTE_LONG:
	    if (! inElement)
	      doChildren = index + 4;
	    int end = getIntN(index+2);
	    index = end + (end < 0 ? data.length + 1: index);
	    break;
	  case POSITION_PAIR_FOLLOWS:
	    {
	      AbstractSequence seq = (AbstractSequence) objects[getIntN(index)];
	      int ipos = getIntN(index+2);
	      ((TreeList) seq).stringValue(inElement, ipos >> 1, sbuf);
	      index += 4;
	    }
	    break;
	  case POSITION_REF_FOLLOWS:
	  case OBJECT_REF_FOLLOWS:
	  default:
	    throw new Error("unimplemented: "+Integer.toHexString(datum)+" at:"+index);
	  }
      }
    if (value != null)
      sbuf.append(value);
    if (doChildren > 0)
      {
	do
	  {
	    doChildren = stringValue(true, doChildren, sbuf);
	  }
	while (doChildren >= 0);
      }
    return index;
  }

  public int createRelativePos(int istart, int offset, boolean isAfter)
  {
    if (isAfter)
      {
	if (offset == 0)
	  {
	    if ((istart & 1) != 0)
	      return istart;
	    if (istart == 0)
	      return 1;
	  }
	offset--;
      }
    if (offset < 0)
      throw unsupported("backwards createRelativePos");
    int pos = posToDataIndex(istart);
    while (--offset >= 0)
      {
	pos = nextDataIndex(pos);
	if (pos < 0)
	  throw new IndexOutOfBoundsException();
      }
    if (pos >= gapEnd)
      pos -= gapEnd - gapStart;
    return isAfter ? ((pos + 1) << 1) | 1 : (pos << 1);
  }

  /** Skip all primitive content nodes. */
  public final int nextNodeIndex (int pos, int limit)
  {
   if ((limit | 0x80000000) == -1) // kludge
     limit = data.length;
    for (;;)
      {
	if (pos == gapStart)
	  pos = gapEnd;
	if (pos >= limit)
	  return pos;
	char datum = data[pos];
	if (datum <= MAX_CHAR_SHORT
	    || (datum >= OBJECT_REF_SHORT
		&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
	    || (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
		&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
	    || (datum & 0xFF00) == BYTE_PREFIX)
	  {
	    pos++;
	    continue;
	  }
	if (datum >= BEGIN_ELEMENT_SHORT
	    && datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
	  return pos;
	switch (datum)
	  {
	  case DOCUMENT_URI:
	    pos += 3;
	    break;
          case JOINER:
            pos += 1;
            break;
	  case PROCESSING_INSTRUCTION:
	  case COMMENT:
	  case BEGIN_DOCUMENT:
	  case BEGIN_ELEMENT_LONG:
	  case BEGIN_ATTRIBUTE_LONG:
	    return pos;
          case BEGIN_ENTITY:
            pos += 5;
            break;
	  case END_ELEMENT_SHORT:
	  case END_ELEMENT_LONG:
	  case END_ATTRIBUTE:
	  case END_DOCUMENT:
	  case END_ENTITY:
	    return pos;
	  case CDATA_SECTION:
	  default:
	    pos = nextDataIndex(pos);
	    continue;
	  }
      }
  }

  public int nextMatching(int startPos, ItemPredicate predicate,
			  int endPos, boolean descend)
  {
    int start = posToDataIndex(startPos);
    int limit = posToDataIndex(endPos);
    int pos = start;
    if (predicate instanceof NodePredicate)
      pos = nextNodeIndex(pos, limit);
    boolean checkAttribute = false; // true if attribute nodes could match.
    boolean checkNode;
    boolean checkText;
    boolean checkElement; // true if element nodes could match.
    if (predicate instanceof ElementPredicate)
      {
	checkNode = true;
	checkElement = true;
	checkText = false;
      }
    else if (predicate instanceof AttributePredicate)
      {
	checkNode = true;
	checkElement = false;
	checkText = false;
      }
    else
      {
	checkNode = true;
	checkElement = true;
	checkText = true;
      }
    int next;
    for (;; pos = next)
      {
	if (pos == gapStart)
	  pos = gapEnd;
	if (pos >= limit && limit != -1)
	  return 0;
	int j;
	char datum = data[pos];
	if (datum <= MAX_CHAR_SHORT
	    || (datum >= OBJECT_REF_SHORT
		&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
	    || (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
		&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT))
	  {
	    if (checkText && predicate.isInstancePos(this, pos << 1))
	      {
		if (pos >= gapEnd)
		  pos -= gapEnd - gapStart;
		return pos << 1;
	      }
	    next = pos + 1;
	    continue;
	  }
	switch (datum)
	  {
	  case DOCUMENT_URI:
	    next = pos + 3;
	    continue;
	  case BEGIN_DOCUMENT:
	    next = pos + 5;
	    if (checkNode) break;
	    continue;
	  case BEGIN_ENTITY:
	    next = pos + 5;
	    continue;
	  case POSITION_REF_FOLLOWS:
	  case OBJECT_REF_FOLLOWS:
	  case INT_FOLLOWS:
	  case FLOAT_FOLLOWS:
	    next = pos + 3;
	    if (checkText) break;
	    continue;
	  case CHAR_FOLLOWS:
	    next = pos + 2;
	    continue;
	  case END_ELEMENT_SHORT:
	    if (! descend)
	      return 0;
	    next = pos + 2;
	    continue;
	  case POSITION_PAIR_FOLLOWS:
	    next = pos + 5;
	    if (checkText) break;
	    continue;
	  case END_ELEMENT_LONG:
	    if (! descend)
	      return 0;
	    next = pos + 7;
	    continue;
	  case END_ATTRIBUTE:
	  case END_DOCUMENT:
	    if (! descend)
	      return 0;
            /* ... fall through ...*/
          case END_ENTITY:
	    next = pos + 1;
	    continue;
	  case BEGIN_ATTRIBUTE_LONG:
	    if (checkNode)
	      {
		j = getIntN(pos+3);
		next = j + 1 + (j < 0 ? data.length : pos);
	      }
	    else
	      next = pos + 5;
	    if (checkAttribute) break;
	    continue;
	  case BOOL_FALSE:
	  case BOOL_TRUE:
	    next = pos + 1;
	    if (checkText) break;
	    continue;
          case JOINER:
	    next = pos + 1;
            continue;
	  case LONG_FOLLOWS:
	  case DOUBLE_FOLLOWS:
	    next = pos + 5;
	    if (checkText) break;
	    continue;	
	  case PROCESSING_INSTRUCTION:
	    next = pos + 5 + getIntN(pos+3);
	    if (checkNode) break;
	    continue;	
	  case COMMENT:
	    next = pos + 3 + getIntN(pos+1);
	    if (checkNode) break;
	    continue;	
	  case CDATA_SECTION:
	    next = pos + 3 + getIntN(pos+1);
	    if (checkText) break;
	    continue;	
	  case BEGIN_ELEMENT_LONG:
	    if (descend)
	      next = pos + 3;
	    else
	      {
		j = getIntN(pos+1);
		next = j + (j < 0 ? data.length :  pos) + 7;
	      }
	    if (checkElement) break;
	    continue;
	  default:
	    if (datum >= BEGIN_ELEMENT_SHORT
		&& datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
	      {
		if (descend)
		  next = pos + 3;
		else
		  next = pos + data[pos+1] + 2;
		if (checkElement) break;
	      }
	    else
	      throw new Error("unknown code:"+(int) datum);
	    continue;
	  }
 	if (pos > start && predicate.isInstancePos(this, pos << 1))
	  {
	    if (pos >= gapEnd)
	      pos -= gapEnd - gapStart;
	    return pos << 1;
	  }
      }
  }

  public int nextPos (int position)
  {
    int index = posToDataIndex(position);
    if (index == data.length)
      return 0;
    if (index >= gapEnd)
      index -= gapEnd - gapStart;
    return (index << 1) + 3;
  }

  public final int nextDataIndex(int pos)
  {
    if (pos == gapStart)
      pos = gapEnd;
    if (pos == data.length)
      return -1;
    int j;
    char datum = data[pos++];
    if (datum <= MAX_CHAR_SHORT
	|| (datum >= OBJECT_REF_SHORT
	    && datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
	|| (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	    && datum <= INT_SHORT_ZERO + MAX_INT_SHORT))
      return pos;
    if (datum >= BEGIN_ELEMENT_SHORT
	&& datum <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
      return data[pos] + pos + 1;
    switch (datum)
      {
      case BEGIN_DOCUMENT:
	j = getIntN(pos);
	j += j < 0 ? data.length : pos-1;
	return  j + 1;
      case BEGIN_ENTITY:
        j = pos + (BEGIN_ENTITY_SIZE-1);
        for (;;)
          {
            if (j == gapStart)
              j = gapEnd;
            if (j == data.length)
              return -1; // actually error.
            if (data[j] == END_ENTITY)
              return j + 1;
            j = nextDataIndex(j);
          }
      case BOOL_FALSE:
      case BOOL_TRUE:
      case JOINER:
	return pos;
      case CHAR_FOLLOWS:
	return pos + 1;
      case POSITION_REF_FOLLOWS:
      case OBJECT_REF_FOLLOWS:
      case INT_FOLLOWS:
      case FLOAT_FOLLOWS:
	return pos + 2;
      case POSITION_PAIR_FOLLOWS:
	return pos + 4;
      case END_ELEMENT_SHORT:
      case END_ELEMENT_LONG:
      case END_ATTRIBUTE:
      case END_DOCUMENT:
      case END_ENTITY:
	return -1;
      case BEGIN_ELEMENT_LONG:
	j = getIntN(pos);
	j += j < 0 ? data.length : pos-1;
	return  j + 7;
      case BEGIN_ATTRIBUTE_LONG:
	j = getIntN(pos+2);
	j += j < 0 ? data.length : pos-1;
	return j + 1;
      case LONG_FOLLOWS:
      case DOUBLE_FOLLOWS:
	return pos + 4;
      case PROCESSING_INSTRUCTION:
	pos += 2;
	// ... fall through ...
      case CDATA_SECTION:
      case COMMENT:
	return pos + 2 + getIntN(pos);
      default:
	throw new Error("unknown code:"+Integer.toHexString((int) datum));
      }
  }

  public Object documentUriOfPos (int pos)
  {
    int index = posToDataIndex(pos);
    if (index == data.length)
      return null;
    if (data[index] == BEGIN_DOCUMENT)
      {
        int next = index + 5;
        if (next == gapStart)
          next = gapEnd;
        if (next < data.length && data[next] == DOCUMENT_URI)
          return objects[getIntN(next+1)];
      }
    return null;
  }

  /** Compare two positions, and indicate their relative order. */
  public int compare(int ipos1, int ipos2)
  {
    // It's difficult to optimize this, because because if (say) isAfter(ipos1)
    // then we need nextDataIndex((ipos1>>>1)-1).  In that case comparing
    // (ipos1>>>1)-1 and (pos2>>>1)-1 tells us nothing, since the former
    // could be a BEGIN_ELEMENT, while the latter might be a node inside
    // the element.
    int i1 = posToDataIndex(ipos1);
    int i2 = posToDataIndex(ipos2);
    return i1 < i2 ? -1 : i1 > i2 ? 1 : 0;
  }

  protected int getIndexDifference(int ipos1, int ipos0)
  {
    int i0 = posToDataIndex(ipos0);
    int i1 = posToDataIndex(ipos1);
    boolean negate = false;
    if (i0 > i1)
      {
	negate = true;
	int i = i1;  i1 = i0;  i0 = i;
      }
    int i = 0;
    while (i0 < i1)
      {
	i0 = nextDataIndex(i0);
	i++;
      }
    return negate ? -i : i;
  }

  public int nextIndex(int ipos)
  {
    return getIndexDifference(ipos, startPos());
  }
  
  public int hashCode()
  {
    // Calculating a real hashCode is real pain.
    return System.identityHashCode(this);
  }

  public void consume(Consumer out)
  {
    consumeIRange(0, data.length, out);
  }

  public void statistics ()
  {
    java.io.PrintWriter out = new java.io.PrintWriter(System.out);
    statistics(out);
    out.flush();
  }

  public void statistics (java.io.PrintWriter out)
  {
    out.print("data array length: ");  out.println(data.length);
    out.print("data array gap: ");  out.println(gapEnd-gapStart);
    out.print("object array length: ");  out.println(objects.length);
  }

  // /* DEBUGGING
  public void dump ()
  {
    java.io.PrintWriter out = new java.io.PrintWriter(System.out);

    dump(out);
    out.flush();
  }

  public void dump (java.io.PrintWriter out)
  {
    out.println(getClass().getName()+" @"+Integer.toHexString(System.identityHashCode(this))
		       + " gapStart:"+gapStart+" gapEnd:"+gapEnd+" length:"+data.length);
    dump(out, 0, data.length);
  }

  public void dump (java.io.PrintWriter out, int start, int limit)
  {
    int toskip = 0;
    // Skip follow-on words.
    boolean skipFollowingWords = true;
    for (int i = start;  i < limit;  i++)
      {
	
	if (i < gapStart || i >= gapEnd)
	  {
	    int j;  long l;
	    int ch = data[i];
            out.print(""+i+": 0x"+Integer.toHexString(ch)+'='+((short) ch));
	    if (--toskip < 0)
	      {
		if (ch <= MAX_CHAR_SHORT)
		  {
		    if (ch >= ' ' && ch < 127)
		      out.print("='"+((char)ch)+"'");
		    else if (ch=='\n')
		      out.print("='\\n'");
		    else
		      out.print("='\\u"+Integer.toHexString(ch)+"'");
		  }
		else if (ch >= OBJECT_REF_SHORT
			 && ch <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
		  {
		    ch = ch - OBJECT_REF_SHORT;
		    Object obj = objects[ch];
		    out.print("=Object#");
                    out.print((int)ch);
                    out.print('=');
		    out.print(obj);
                    if (obj != null) {
                      out.print(':');
                      out.print(obj.getClass().getName());
                    }
                    out.print('@');
                    out.print(Integer.toHexString(System.identityHashCode(obj)));
		  }
		else if (ch >= BEGIN_ELEMENT_SHORT
			 && ch <= BEGIN_ELEMENT_SHORT+BEGIN_ELEMENT_SHORT_INDEX_MAX)
		  {
		    ch = ch - BEGIN_ELEMENT_SHORT;
		    j = data[i+1] + i;
		    out.print("=BEGIN_ELEMENT_SHORT end:"+j+" index#"+((int)ch)+"=<"+objects[ch]+'>');
		    toskip = 2;
		  }
		else if (ch >= INT_SHORT_ZERO + MIN_INT_SHORT
			 && ch <= INT_SHORT_ZERO + MAX_INT_SHORT)
		  {
		    out.print("= INT_SHORT:"+(ch-INT_SHORT_ZERO));
		  }
		else
		  {
		    switch (ch)
		      {
		      case INT_FOLLOWS:
			j = getIntN(i+1);
			out.print("=INT_FOLLOWS value:"+j);
			toskip = 2;
			break;
		      case LONG_FOLLOWS:
			l = getLongN(i+1);
			out.print("=LONG_FOLLOWS value:"+l);
			toskip = 4;
			break;
		      case FLOAT_FOLLOWS:
			j = getIntN(i+1);
			out.write("=FLOAT_FOLLOWS value:"
				  +Float.intBitsToFloat(j));
			toskip = 2;
			break;
		      case DOUBLE_FOLLOWS:
			l = getLongN(i+1);
			out.print("=DOUBLE_FOLLOWS value:"
				  +Double.longBitsToDouble(l));
			toskip = 4;
			break;
		      case BEGIN_DOCUMENT:
			j = getIntN(i+1);
			j += j < 0 ? data.length : i;
			out.print("=BEGIN_DOCUMENT end:");
			out.print(j);
                        out.print(" parent:");
			j = getIntN(i+3);
			out.print(j);
			toskip = 4;
			break;
		      case BEGIN_ENTITY:
			j = getIntN(i+1);
			out.print("=BEGIN_ENTITY base:");
			out.print(j);
                        out.print(" parent:");
			j = getIntN(i+3);
			out.print(j);
			toskip = 4;
			break;
		      case END_ENTITY:
			out.print("=END_ENTITY");
			break;
		      case DOCUMENT_URI:
			out.print("=DOCUMENT_URI: ");
			j = getIntN(i+1);
			out.print(objects[j]);
			toskip = 2;
			break;
		      case COMMENT:
			out.print("=COMMENT: '");
			j = getIntN(i+1);
			out.write(data, i+3, j);
			out.print('\'');
			toskip = 2+j;
			break;
		      case CDATA_SECTION:
			out.print("=CDATA: '");
			j = getIntN(i+1);
			out.write(data, i+3, j);
			out.print('\'');
			toskip = 2+j;
			break;
		      case PROCESSING_INSTRUCTION:
			out.print("=PROCESSING_INSTRUCTION: ");
			j = getIntN(i+1);
			out.print(objects[j]);
			out.print(" '");
			j = getIntN(i+3);
			out.write(data, i+5, j);
			out.print('\'');
			toskip = 4+j;
			break;
		      case END_DOCUMENT:
			out.print("=END_DOCUMENT");
			break;
		      case BOOL_FALSE: out.print("= false");  break;
		      case BOOL_TRUE:  out.print("= true");  break;
		      case JOINER:  out.print("= joiner");  break;
		      case CHAR_FOLLOWS:
			out.print("=CHAR_FOLLOWS"); toskip = 1;  break;
		      case POSITION_REF_FOLLOWS:
		      case OBJECT_REF_FOLLOWS:
			toskip = 2;  break;
		      case END_ELEMENT_SHORT:
			out.print("=END_ELEMENT_SHORT begin:");
			j = i - data[i+1];
			out.print(j);
			j = data[j] - BEGIN_ELEMENT_SHORT;
			out.print(" -> #");
			out.print(j);
			out.print("=<");
			out.print(objects[j]);
			out.print('>');
			toskip = 1;  break;
		      case BEGIN_ELEMENT_LONG:
			j = getIntN(i+1);
			j += j < 0 ? data.length : i;
			out.print("=BEGIN_ELEMENT_LONG end:");
			out.print(j);
			j = getIntN(j + 1);
			out.print(" -> #");
			out.print(j);
                        if (j >= 0 && j+1 < objects.length)
                          out.print("=<"+objects[j]+'>');
                        else
                          out.print("=<out-of-bounds>");
			toskip = 2;
			break;
		      case END_ELEMENT_LONG:
			j = getIntN(i+1);
			out.print("=END_ELEMENT_LONG name:"+j
				  +"=<"+objects[j]+'>');
			j = getIntN(i+3);
			j = j < 0 ? i + j : j;
			out.print(" begin:"+j);
			j = getIntN(i+5);
			j = j < 0 ? i + j : j;
			out.print(" parent:"+j);
			toskip = 6;
			break;
		      case BEGIN_ATTRIBUTE_LONG:
			j = getIntN(i+1);
			out.print("=BEGIN_ATTRIBUTE name:"+j
				  +"="+objects[j]);
			j = getIntN(i+3);
			j += j < 0 ? data.length : i;
			out.print(" end:"+j);
			toskip = 4;
			break;
		      case END_ATTRIBUTE: out.print("=END_ATTRIBUTE"); break;
		      case POSITION_PAIR_FOLLOWS:
			out.print("=POSITION_PAIR_FOLLOWS seq:");
			{
			  j = getIntN(i+1);  out.print(j);
                          out.print('=');
			  Object seq = objects[j];
                          out.print(seq==null?null:seq.getClass().getName());
                          out.print('@');
			  if (seq == null) out.print("null");
			  else out.print(Integer.toHexString(System.identityHashCode(seq)));
			  out.print(" ipos:");
			  out.print(getIntN(i+3));
			}
			toskip = 4;
			/*
			AbstractSequence seq = (AbstractSequence) objects[getIntN(i+1)];
			ipos = getIntN(i+3);
			*/
			break;
		      }
		  }
	      }
	    out.println();
	    if (skipFollowingWords && toskip > 0)
	      {
		i += toskip;
		toskip = 0;
	      }
	  }
      }
  }
  // DEBUGGING */
}
