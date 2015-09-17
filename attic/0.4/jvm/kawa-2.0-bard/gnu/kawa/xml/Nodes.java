// Copyright (c) 2003, 2013  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.Values;
import gnu.lists.*;
import gnu.xml.*;
/* #ifdef use:org.w3c.dom.Node */
import org.w3c.dom.*;
/* #endif */

/** Manages a sequence of node references.
 */

public class Nodes extends Values.FromList<SeqPosition>
    implements PositionConsumer,
               /* #ifdef use:org.w3c.dom.Node */
               org.w3c.dom.NodeList,
               /* #endif */
               Consumer
{
    protected GapVector<SeqPosition> vector;
    protected NodeVector nvector;

    private Nodes(GapVector<SeqPosition> vector) {
        super(vector);
        this.vector = vector;
    }

    private Nodes(NodeVector nvector) {
        this(new GapVector<SeqPosition>(nvector));
        this.nvector = nvector;
    }

    public Nodes() {
        this(new NodeVector());
    }

    public Consumer append(char c) {
        maybeStartTextNode();
        curFragment.append(c);
        return this;
    }

    public Consumer append(CharSequence csq) {
        maybeStartTextNode();
        curFragment.append(csq);
        return this;
    }

    public boolean ignoring() {
        return false;
    }

    int nesting = 0;
    boolean inAttribute;
    NodeTree curNode;
    XMLFilter curFragment;

    public void writePosition (AbstractSequence seq, int ipos) {
        nvector.writePosition(seq, ipos);
    }
  
    public void writePosition(SeqPosition position) {
        nvector.writePosition(position);
    }

    public void writeObject(Object v) {
        if (curFragment != null) {
            if (nesting == 0
                && (v instanceof SeqPosition || v instanceof TreeList))
                finishFragment();
            else {
                curFragment.writeObject(v);
                return;
            }
        }
        if (v instanceof SeqPosition) {
            writePosition((SeqPosition) v);
            return;
        }
        if (v instanceof TreeList) {
            TreeList tlist = (TreeList) v;
            writePosition(tlist, 0);
            return;
        }
        handleNonNode();
        curFragment.writeObject(v);
        return;
    }

  void maybeStartTextNode ()
  {
    if (curFragment == null)
      {
        throw new IllegalArgumentException("non-node where node required");
      }
  }

  void handleNonNode ()
  {
    if (curFragment == null)
      {
        throw new ClassCastException("atomic value where node is required");
      }
  }

  public void writeFloat (float v)
  {
    handleNonNode();
    curFragment.writeFloat(v);
  }

  public void writeDouble (double v)
  {
    handleNonNode();
    curFragment.writeDouble(v);
  }

  public void writeLong(long v)
  {
    handleNonNode();
    curFragment.writeLong(v);
  }

  public void writeInt(int v)
  {
    handleNonNode();
    curFragment.writeInt(v);
  }

  public void writeBoolean (boolean v)
  {
    handleNonNode();
    curFragment.writeBoolean(v);
  }

  public void write (int v)
  {
    maybeStartTextNode();
    curFragment.write(v);
  }

    public Consumer append (CharSequence csq, int start, int end) { 
        maybeStartTextNode();
        curFragment.append(csq, start, end);
        return this;
    }

  public void write(char[] buf, int off, int len)
  {
    maybeStartTextNode();
    curFragment.write(buf, off, len);
  }

  public void write(CharSequence str, int start, int length)
  {
    maybeStartTextNode();
    curFragment.write(str, start, length);
  }

  public void write (String str)
  {
    maybeStartTextNode();
    curFragment.write(str);
  }

  private void maybeStartNonTextNode ()
  {
    if (curFragment != null && nesting == 0)
      finishFragment();
    if (curFragment == null)
      startFragment();
    nesting++;
  }

  private void maybeEndNonTextNode ()
  {
    if (--nesting == 0)
      finishFragment();
  }

  public void startElement (Object type)
  {
    maybeStartNonTextNode();
    curFragment.startElement(type);
  }

  public void endElement ()
  {
    curFragment.endElement();
    maybeEndNonTextNode();
  }

  public void startAttribute(Object attrType)
  {
    maybeStartNonTextNode();
    curFragment.startAttribute(attrType);
    inAttribute = true;
  }

  public void endAttribute()
  {
    if (! inAttribute)
      return;
    inAttribute = false;
    curFragment.endAttribute();
    maybeEndNonTextNode();
  }

  public void writeComment(char[] chars, int offset, int length)
  {
    maybeStartNonTextNode();
    curFragment.writeComment(chars, offset, length);
    maybeEndNonTextNode();
  }

  public void writeCDATA(char[] chars, int offset, int length)
  {
    maybeStartNonTextNode();
    curFragment.writeCDATA(chars, offset, length);
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    maybeStartNonTextNode();
    curFragment.writeProcessingInstruction(target, content, offset, length);
    maybeEndNonTextNode();
  }

  public void startDocument()
  {
    maybeStartNonTextNode();
    curFragment.startDocument();
  }

  public void endDocument()
  {
    curFragment.endDocument();
    maybeEndNonTextNode();
  }

  public void beginEntity(Object base)
  {
    maybeStartNonTextNode();
    curFragment.beginEntity(base);
  }

  public void endEntity()
  {
    curFragment.endEntity();
    maybeEndNonTextNode();
  }

  void startFragment ()
  {
    curNode = new NodeTree();
    curFragment = new XMLFilter(curNode);
    writePosition(curNode, 0);
  }

  void finishFragment ()
  {
    curNode = null;
    curFragment = null;
  }

  /*
  public int size()
  {
    return count;
  }
  */

  public int getLength()
  {
    return size();
  }

  /* #ifdef use:org.w3c.dom.Node */
  public Node item(int index)
  {
    if (index >= size())
      return null;
    else
      return (Node) get(index);
  }
  /* #endif */

    /** Optimization of ((SeqPosition) get(index)).sequence.
     * However returns null instead of throwing IndexOutOfBoundsException
     * if index >= count. */
    public AbstractSequence getSeq(int index) {
        if (index >= vector.gapStart) {
            index += vector.gapEnd - vector.gapStart;
            if (index >= nvector.size())
                return null;
        }
        return nvector.getSeq(index);
    }

    /** Optimization of ((SeqPosition) get(index)). ipos. */
    public int getPos(int index) {
        if (index >= vector.gapStart)
            index += vector.gapEnd - vector.gapStart;
        return nvector.getPos(index);
    }

    public void consumePosRange (int iposStart, int iposEnd, Consumer out) {
        vector.consumePosRange(iposStart, iposEnd, out);
    }

    public static class NodeVector
        extends SimpleVector<SeqPosition>
        implements PositionConsumer {
        Object[] odata;
        int[] idata;

        public int getBufferLength() {
            return odata == null ? 0 : odata.length;
        }

        public void setBufferLength(int length) {
            checkCanWrite();
            int oldLength = odata == null ? 0 : odata.length;
            if (oldLength != length) {
                if (oldLength > length)
                    oldLength = length;
                Object[] otmp = new Object[length];
                int[] itmp = new int[length];
                if (oldLength != 0) {
                    System.arraycopy(odata, 0, otmp, 0, oldLength);
                    System.arraycopy(idata, 0, itmp, 0, oldLength);
                }
                odata = otmp;
                idata = itmp;
            }
        }

        protected Object getBuffer() { throw new Error(); }

        protected SeqPosition getBuffer(int index) {
            Object obj = odata[index];
            if (obj instanceof SeqPosition)
                return (SeqPosition) obj;
            return makeSeqPos((AbstractSequence) obj, idata[index]);
        }

        public AbstractSequence getSeq(int index) {
            Object obj = odata[index];
            if (obj instanceof SeqPosition)
                return ((SeqPosition) obj).sequence;
            return (AbstractSequence) obj;
        }

        public int getPos(int index) {
            Object obj = odata[index];
            if (obj instanceof SeqPosition)
                return ((SeqPosition) obj).ipos;
            return idata[index];
        }

        protected SeqPosition makeSeqPos(AbstractSequence seq, int ipos) {
            if (seq instanceof NodeTree)
                return KNode.make((NodeTree) seq, ipos);
            else
                return new SeqPosition(seq, ipos);
        }

        protected void setBuffer(int index, SeqPosition value) {
            checkCanWrite();
            odata[index] = value;
            idata[index] = 0;
        }

        protected void setBuffer(int index, AbstractSequence seq, int ipos) {
            checkCanWrite();
            odata[index] = seq;
            idata[index] = ipos;
        }

        protected void clearBuffer(int start, int count) {
            checkCanWrite();
            Object[] d = odata;
            while (--count >= 0)
                d[start++] = null;
        }

        public void writePosition(SeqPosition seq) {
            add(seq);
        }

        public void writePosition(AbstractSequence seq, int ipos) {
            int sz = size;
            add((SeqPosition) null);
            odata[sz] = seq;
            idata[sz] = ipos;
        }

        public void shift(int srcStart, int dstStart, int count) {
            checkCanWrite();
            System.arraycopy(odata, srcStart, odata, dstStart, count);
            System.arraycopy(idata, srcStart, idata, dstStart, count);
        }

        public void consumePosRange (int iposStart, int iposEnd, Consumer out) {
            if (out.ignoring())
                return;
            int i = iposStart >>> 1;
            int end = iposEnd >>> 1;
            if (end > size)
                end = size;
            for (;  i < end;  i++) {
                if (out instanceof PositionConsumer) {
                    PositionConsumer pout = (PositionConsumer) out;
                    Object obj = odata[i];
                    if (obj instanceof SeqPosition)
                        pout.writePosition((SeqPosition) obj);
                    else
                        pout.writePosition((AbstractSequence) obj, idata[i]);
                }
                else
                    out.writeObject(getBuffer(i));
            }
        }
    }

  public static KNode root (NodeTree seq, int ipos)
  {
    int root;
    if (seq.gapStart > TreeList.BEGIN_ENTITY_SIZE
        && seq.data[0] == TreeList.BEGIN_ENTITY)
      root = TreeList.BEGIN_ENTITY_SIZE << 1;
    else
      root = 0;
    return KNode.make(seq, root);
  }
}
