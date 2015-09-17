package gnu.mapping;
import java.io.*;
import gnu.lists.*;
import gnu.text.Printable;
import java.util.*;

/** Encapsulate multiple values in a single object.
 * In Scheme and Lisp mainly used to return multiple values from a function.
 * In XQuery used to represent a non-singleton sequence.
 */

public abstract class Values<E>
    extends AbstractSequence<E> 
    implements Consumable, Externalizable                                    
{
    public static final Object[] noArgs = new Object[0];

    public static final Values empty = FromArray.empty;

    protected Values () {
    }

    /** Get the values encapsulated. */
    public Object[] getValues() {
        int sz = size();
        Object[] arr = new Object[sz];
        for (int it = 0, i = 0; (it = nextPos(it)) != 0; ) {
          arr[i++] = getPosPrevious(it);
        }
        return arr;
    }
    
    public static Object values(Object... vals) {
        return make(vals);
    }
    
    public static <E,V1 extends E,V2 extends E> Values2<E,V1,V2> values2(V1 val1, V2 val2) {
        return new Values2<E,V1,V2>(val1, val2);
    }

    public static Values make() {
        return new FromTreeList();
    }

    /** Create a value for each element of an array.
     * 
     * @param vals values to use. 
     *    The array should be immutable, as may be re-used for the result.
     */
    public static <E> Object make(E[] vals) {
        if (vals.length == 1)
            return vals[0];
        else if (vals.length == 0)
            return empty;
        else {
            return new FromArray<E>(vals);  
        }
    }

    /** Create a value for each element of a list.
     * 
     * @param seq values to use. 
     *    The list should be immutable, as may be re-used for the result.
     */
    public static <E> Object make(List<E> seq) {
        int count = seq == null ? 0 : seq.size();
        if (count == 0)
            return empty;
        if (count == 1)
            return seq.get(0);
        return new FromList<E>(seq);
    }

    public static Object make(TreeList list) {
        return make(list, 0, list.data.length);
    }

    /**
     * Extract a value (single or Values) from a sub-range of a TreeList.
     * @param list the TreeList to copy
     * @param startPosition start of range, as a raw index in data
     * @param endPosition end of range, as a raw index in data
     */
    public static Object make(TreeList list, int startPosition, int endPosition) {
        int next;
        if (startPosition == endPosition
            || (next = list.nextDataIndex(startPosition)) <= 0)
            return empty;
        if (next == endPosition || list.nextDataIndex(next) < 0)
            return list.getPosNext(startPosition << 1); // Singleton value
        FromTreeList vals = new FromTreeList();
        list.consumeIRange(startPosition, endPosition, vals.buffer);
        return vals;
    }

    /** If a simple value, return that value.
     * Also, if no values, return empty.
     */
    public Object canonicalize() {
        int sz = size();
        if (sz == 0)  return Values.empty;
        if (sz == 1)  return get(0);
        return this;
    }

    /** Apply a Procedure with these values as the arguments. */
    public Object call_with (Procedure proc) throws Throwable {
        return proc.applyN(getValues());
    }

    public void check_with(Procedure proc, CallContext ctx) {
        proc.checkN(getValues(), ctx);
    }

    public void print(Consumer out) {
        if (this == empty) {
            out.write("#!void");
            return;
        }
        boolean readable = true;  // FIXME
        if (readable)
            out.write("#<values");
        for (int it = 0; (it = nextPos(it)) != 0; ) {
            out.write(' ');
            Object val = getPosPrevious(it);
            if (val instanceof Printable)
                ((Printable) val).print(out);
            else
                out.writeObject(val);
        }
        if (readable)
            out.write('>');
    }

    /**
     * @serialData Write the length (using writeInt), followed by
     *   the values in order (written using writeObject).
     */
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeInt(size());
        for (int it = 0, i = 0; (it = nextPos(it)) != 0; ) {
            out.writeObject(getPosPrevious(it));
        }
    }

    public Object readResolve() throws ObjectStreamException {
        return canonicalize();
    }

    /** Helper method called by compiled code.
     * The compiled code iterates through zero or more values.
     * Return the index of the next value, or -1 if currently at eof.
     * A non-Values object is treated as a singleton value,
     * so in that case there is no next value.
     */
    public static int nextIndex(Object values, int curIndex) {
        if (values instanceof Values) {
            if (curIndex == Integer.MAX_VALUE)
                curIndex = -1;
            int next = ((Values) values).nextPos(curIndex);
            return next == 0 ? -1 : next == -1 ? Integer.MAX_VALUE : next;
        } else
            return curIndex == 0 ? 1 : -1;
    }

    /** Helper method called by compiled code.
     * The compiled code iterates through zero or more values.
     * Extract the object referenced by the curIndex.
     * A non-Values object is treated as a singleton value.
     */
    public static Object nextValue(Object values, int curIndex) {
        if (values instanceof Values) {
	    if (curIndex == Integer.MAX_VALUE)
                curIndex = -1;
	    return ((Values) values).getPosNext(curIndex);
        } else
            return values;
    }
    
    protected int nextIndex(int ipos) {
      if (ipos==-1) return size();
        return ipos >>> 1;
    }

    public static void writeValues(Object value, Consumer out) {
        if (value instanceof Values) {
            ((Values) value).consume(out);
            /*
            Values vals = (Values) value;
            for (int iter = 0;
                  (iter = vals.nextPos(iter)) != 0; ) {
               writeValues(vals.getPosPrevious(iter), out);
            */
        }
        /*else if (value instanceof Consumable) {
	    ((Consumable) value).consume(out);
        }
        */
        else
            out.writeObject(value);
    }

    public static int countValues(Object value) {
        return value instanceof Values ? ((Values) value).size() : 1;
    }

    /** An implementation of Values that stores the values in an array.
     */
    public static class FromArray<E> extends Values<E> {
        E[] data;

        public FromArray(E[] data) {
            this.data = data;
        }

        public static final FromArray<Object> empty
            = new FromArray<Object>(noArgs);

        @Override
        public int size() {
            return data.length;
        }

        @Override
        public E get(int index) {
            return data[index];
        }

        @Override
        public Object[] getValues() {
            return data;
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            int len = in.readInt();
            E[] arr = (E[]) new Object[len];
            for (int i = 0;  i < len;  i++)
                arr[i] = (E) in.readObject();
            data = arr;
        }

    }

    /** An implementation of Values that uses a java.util.List.
     */
    public static class FromList<E> extends Values<E> {
        private List<E> list;
        
        public FromList(List<E> list) {
            this.list = list;
        }

        @Override
        public int size() {
            return list.size();
        }

        @Override
        public E get(int index) {
            return list.get(index);
        }

        @Override
        public Object[] getValues() {
          return list.toArray();
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            int len = in.readInt();
            ArrayList<E> lst = new ArrayList<E>(len);
            for (int i = 0;  i < len;  i++)
                lst.add((E) in.readObject());
            list = lst;
        }
    }

    /** A specialization of Values for exactly 2 values.
     */
    public static class Values2<E, V1 extends E, V2 extends E> extends Values<E> {
        V1 value1;
        V2 value2;
        public Values2(V1 value1, V2 value2) {
            this.value1 = value1;
            this.value2 = value2;
        }
        public V1 getValue1() { return value1; }
        public V2 getValue2() { return value2; }

        @Override
        public Object call_with(Procedure proc) throws Throwable {
            return proc.apply2(value1, value2);
        }

        @Override
        public void check_with(Procedure proc, CallContext ctx) {
            proc.check2(value1, value2, ctx);
        }

        @Override
        public int size() {
            return 2;
        }

        @Override
        public E get(int index) {
            if (index == 0)  return value1;
            if (index == 1)  return value2;
            throw new IndexOutOfBoundsException();
        }
        
        @Override
        public Object[] getValues() {
          return new Object[] { value1, value2 };
        }
        
        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            int len = in.readInt();
            if (len != 2)
                throw new IOException("inconsistent readExternal");
            value1 = (V1) in.readObject();
            value2 = (V2) in.readObject();
        }

        @Override
        public Object canonicalize() {
            return this;
        }
    }

    /** An implementation of Values that uses a TreeList.
     */
    public static class FromTreeList extends Values<Object>
        implements Printable, Consumer, PositionConsumer /*, Externalizable FIXME*/ {
        protected final TreeList buffer;

        public FromTreeList(Object[] values) {
            this();
            for (int i = 0;  i < values.length;  i++)
                buffer.writeObject(values[i]);
        }
        
        public FromTreeList() {
            buffer = new TreeList();
        }

        public FromTreeList(TreeList list) {
            buffer = list;
        }

        @Override
        public int size() {
            return buffer.size();
        }

        @Override
        public Object get(int index) {
            return buffer.get(index);
        }

        @Override
        public int createPos(int index, boolean isAfter) {
            return buffer.createPos(index, isAfter);
        }

        @Override
        public Object canonicalize() {
            if (buffer.gapEnd == buffer.data.length) {
	        if (buffer.gapStart == 0)
	            return empty;
	        if (buffer.nextDataIndex(0) == buffer.gapStart) // Singleton value.
	            return buffer.getPosNext(0);
            }
            return this;
        }

        @Override
        public Object[] getValues() {
            return buffer.isEmpty() ? noArgs : buffer.toArray();
        }

        @Override
        public int nextMatching(int startPos, ItemPredicate type,
			  int endPos, boolean descend) {
            return buffer.nextMatching(startPos, type, endPos, descend);
        }
        
        @Override
        public void clear() { buffer.clear(); }
        
        @Override
        public int createRelativePos(int pos, int delta, boolean isAfter) {
            return buffer.createRelativePos(pos, delta, isAfter);
        }

        protected int nextIndex(int ipos) {
            return buffer.nextIndex(ipos);
        }
        @Override
        public boolean hasNext(int ipos) {
            return buffer.hasNext(ipos);
        }
        
        @Override
        public int getNextKind(int ipos) {
            return buffer.getNextKind(ipos);
        }
        
        @Override
        public Object getNextTypeObject(int ipos) {
            return buffer.getNextTypeObject(ipos);
        }
        
        @Override
        public int nextPos(int ipos) {
            return buffer.nextPos(ipos);
        }
        @Override
        public int firstChildPos(int ipos) {
            return buffer.firstChildPos(ipos);
        }
        @Override
        public int firstAttributePos (int ipos) {
            return buffer.firstAttributePos(ipos);
        }
        @Override
        public int parentPos(int ipos) {
            return buffer.parentPos(ipos);
        }
        @Override
        public boolean gotoAttributesStart(TreePosition pos) {
            return buffer.gotoAttributesStart(pos);
        }
        @Override
        public Object getPosNext(int ipos) {
            return buffer.getPosNext(ipos);
        }
        @Override
        public Object getPosPrevious(int ipos) {
            return buffer.getPosPrevious(ipos);
        }
        @Override
        public int compare(int ipos1, int ipos2) {
            return buffer.compare(ipos1, ipos2);
        }
        @Override
        public int hashCode() {
            return buffer.hashCode();
        }
        @Override
        public boolean consumeNext(int ipos, Consumer out) {
            return buffer.consumeNext(ipos, out);
        }
        @Override
        public void consumePosRange(int startPos, int endPos, Consumer out) {
            buffer.consumePosRange(startPos, endPos, out);
        }
        @Override
        public void consume(Consumer out) {
            buffer.consume(out);
        }
        @Override
        public void toString (String sep, StringBuffer sbuf) {
            buffer.toString(sep, sbuf);
        }
  
        public void writeBoolean(boolean v) { buffer.writeBoolean(v); }
        public void writeFloat(float v) { buffer.writeFloat(v); }
        public void writeDouble(double v) { buffer.writeDouble(v); }
        public void writeInt(int v) { buffer.writeInt(v); }
        public void writeLong(long v) { buffer.writeLong(v); }
        public void startDocument() { buffer.startDocument(); }
        public void endDocument() { buffer.endDocument(); }
        public void startElement(Object type) { buffer.startElement(type); }
        public void endElement() { buffer.endElement(); }
        public void startAttribute(Object t) { buffer.startAttribute(t); }
        public void endAttribute() { buffer.endAttribute(); }
        public void writeObject(Object v) { buffer.writeObject(v); }
        public boolean ignoring() { return buffer.ignoring(); }
        public void write(int ch) { buffer.write(ch); }
        public void write(String string) { buffer.write(string); }
        public void write(CharSequence s, int i, int l) { buffer.write(s, i, l); }
        public void write(char[] b, int s, int l) { buffer.write(b, s, l); }
        public Consumer append (char c) { return buffer.append(c); }
        public Consumer append (CharSequence csq) { return buffer.append(csq); }
        public Consumer append (CharSequence csq, int start, int end) { return buffer.append(csq, start, end); }
        public void writePosition(SeqPosition spos) { buffer.writePosition(spos); }
        public void writePosition(AbstractSequence seq, int ipos) {
            buffer.writePosition(seq, ipos);
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            int len = in.readInt();
            for (int i = 0;  i < len;  i++)
                writeObject(in.readObject());
        }
    }
}
