package gnu.kawa.io;
import java.io.*;
import gnu.text.*;
import gnu.lists.ByteVector;
import java.nio.*;
import java.nio.charset.*;

/** An InPort that wraps an InputStream.
 * Similar functionality as using an InputStreamReader, but provides hooks
 * to read at the byte level, possibly before setting the charset.
 */

public class BinaryInPort extends InPort {

    private NBufferedInputStream bstrm;
    CharBuffer cbuf = null;

    Charset cset;
    CharsetDecoder decoder;
    Charset csetDefault;

    /** Return the character set used for text.  May be null if not set. */
    public Charset getCharset() { return cset; }

    public void setCharset(Charset cset) {
        this.cset = cset;
        this.decoder = cset.newDecoder();
    }

    public void setCharset(String name) {
        Charset cset = Charset.forName(name);
        if (this.cset == null)
            setCharset(cset);
        else if (! cset.equals(this.cset))
            throw new RuntimeException("encoding "+name+" does not match previous "+this.cset);
    }

    /** Set the charset to use if we don't find other information.
     * Typically: if we don't see a byte-order-mark or a (language-dependent)
     * encoding declaration.  Defaults to UTF-8.
     */
    public void setDefaultCharset(Charset cset) { csetDefault = cset; }

    private BinaryInPort(NBufferedInputStream bstrm, Path path) {
        super(bstrm, path);
        this.bstrm = bstrm;
        setKeepFullLines(false);
    }
    
    public BinaryInPort(InputStream strm) {
        this(new NBufferedInputStream(strm), null);
    }

    public BinaryInPort(InputStream strm, Path path) {
        this(new NBufferedInputStream(strm), path);
    }

    public BinaryInPort(byte[] buffer, int length, Path path) {
        this(new NBufferedInputStream(buffer, length), path);
    }

    /** Buffer must have room for a surrogate pair. */
    @Override
    public void setBuffer(char[] buffer) throws java.io.IOException {
        super.setBuffer(buffer);
        if (limit - pos + 2 < this.buffer.length)
            throw new java.io.IOException("setBuffer - too short");
    }

    /** Check if we're looking at a Byte Order Mark.
     * If set, set encoding, and set position to just after the mark.
     */
    public boolean setFromByteOrderMark()  throws IOException {
        String enc = bstrm.checkByteOrderMark();
        if (enc == null)
            return false;
        setCharset(enc);
        return true;
    }

    public InputStream getInputStream() {
        return bstrm;
    }

    public void resetStart(int pos) throws java.io.IOException {
        bstrm.bbuf.position(pos);
    }

    @Override
    protected int fill(int len) throws java.io.IOException {
        if (cset == null) {
            byte[] barr = bstrm.barr;
            ByteBuffer bbuf = bstrm.bbuf;
            int count = 0;
            int bpos = bbuf.position();
            int blim = bbuf.limit();
            for (;;) {
                if (count >= len) {
                    bbuf.position(bpos);
                    return count;
                }
                if (bpos >= blim) {
                    bbuf.position(bpos);
                    if (count > 0)
                        return count;
                    int nb = bstrm.fillBytes();
                    if (nb < 0)
                        return -1;
                    bpos = bbuf.position();
                    blim = bbuf.limit();
                }
                byte b = barr[bpos];
                if (b >= 0) {
                    // If it's an ASCII character we accept it.
                    buffer[pos+count] = (char) b;
                    bpos++;
                    count++;
                } else if (count > 0) {
                    bbuf.position(bpos);
                    return count;
                } else {
                    /* #ifdef JAVA7 */
                    setCharset(csetDefault != null ? csetDefault
                               : StandardCharsets.UTF_8);
                    /* #else */
                    // if (csetDefault != null)
                    //     setCharset(csetDefault);
                    // else
                    //     setCharset("UTF-8");
                    /* #endif */
                    break;
                }
            }
        }
        if (cbuf == null) {
            cbuf = CharBuffer.wrap(buffer);
        }
        cbuf.limit(pos+len);
        cbuf.position(pos);
        int count;

        for (;;) {
            CoderResult cres = decoder.decode(bstrm.bbuf, cbuf, inEofSeen);
            count = cbuf.position() - pos;
            if (count > 0 || inEofSeen || ! cres.isUnderflow())
                break;
            int rem = bstrm.bbuf.remaining();
            int n = bstrm.fillBytes();
            if (n < 0) {
                inEofSeen = true;
            }
        }
        return count == 0 && inEofSeen ? -1 : count;
    }

    private boolean inEofSeen;

    public int readByte() throws IOException {
        return bstrm.read();
    }

    public int peekByte() throws IOException {
        return bstrm.peek();
    }

    public int readBytes(byte[] buf, int offset, int count)
            throws IOException {
        return bstrm.read(buf, offset, count);
    }

    @Override
    public void close() throws IOException {
        if (bstrm != null)
            bstrm.close();
        bstrm = null;
        super.close();
    }

    @Override
    protected boolean sourceReady() throws IOException {
        return bstrm.ready();
    }

    public static BinaryInPort openFile(Object fname)
        throws IOException {
        Path path = Path.valueOf(fname);
        BinaryInPort p = new BinaryInPort(path.openInputStream(), path);
        p.setCharset("ISO-8859-1");
        return p;
    }

    public static BinaryInPort openHeuristicFile(InputStream strm, Path path)
            throws java.io.IOException {
        NBufferedInputStream bstrm =
            strm instanceof NBufferedInputStream ? (NBufferedInputStream) strm
            : new NBufferedInputStream(strm);
        BinaryInPort inp = new BinaryInPort(bstrm, path);
        inp.setFromByteOrderMark();
        inp.setKeepFullLines(true);
        inp.setConvertCR(true);
        return inp;
    }

}
