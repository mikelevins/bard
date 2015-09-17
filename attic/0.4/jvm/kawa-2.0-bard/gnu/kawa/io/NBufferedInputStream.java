package gnu.kawa.io;

import java.io.*;
import java.nio.*;

/** Similar to a BufferedInputStream, but uses a ByteBuffer. */

public class NBufferedInputStream extends InputStream {
    InputStream base;
    byte[] barr;
    ByteBuffer bbuf;

    public NBufferedInputStream(InputStream base) {
        this.base = base;
        barr = new byte[8192];
        bbuf = ByteBuffer.wrap(barr, 0, 0);
    }

    public NBufferedInputStream(byte[] buffer, int length) {
        barr = buffer;
        bbuf = ByteBuffer.wrap(buffer, 0, length);
    }

    public synchronized int peek() throws IOException {
        int r = read();
        if (r >= 0)
            bbuf.position(bbuf.position()-1);
        return r;
    }

    @Override
    public synchronized int read() throws java.io.IOException {
        if (! bbuf.hasRemaining()) {
            int n = fillBytes();
            if (n <= 0)
                return -1;
        }
        return bbuf.get() & 0xFF;
    }

    @Override
    public synchronized int read(byte[] buf, int offset, int count)
        throws IOException {
        int remaining = bbuf.remaining();
        if (remaining == 0) {
            int n = fillBytes();
            if (n <= 0)
                return -1;
            remaining = bbuf.remaining();
        }
        if (count > remaining)
            count = remaining;
        bbuf.get(buf, offset, count);
        return count;
    }

    synchronized int fillBytes() throws java.io.IOException {
        if (base == null)
            return -1;
        int wpos = bbuf.limit();
        int avail = bbuf.capacity() - wpos;
        if (avail == 0) {
            bbuf.compact();
            bbuf.flip();
            wpos = bbuf.limit();
            avail = bbuf.capacity() - wpos;
        }
        int n = base.read(barr, wpos, avail);
        bbuf.limit(wpos + (n < 0 ? 0 : n));
        return n;
    }

    public synchronized boolean ready() throws IOException {
        return bbuf.hasRemaining()
            || (base != null && base.available() > 0);
    }

    @Override
    public synchronized int available() throws IOException {
        return bbuf.remaining() + (base == null ? 0 : base.available());
    }

    @Override
    public boolean markSupported() {
        return true;
    }

    @Override
    public synchronized void mark(int readLimit) {
        if (readLimit > bbuf.capacity() - bbuf.position()) {
            bbuf.compact();
            bbuf.flip();
            if (readLimit > barr.length) {
                byte[] tmp = new byte[readLimit];
                System.arraycopy(barr, 0, tmp, 0, barr.length);
                barr = tmp;
                bbuf = ByteBuffer.wrap(tmp, 0, bbuf.limit());
            }
        }
        bbuf.mark();
    }

    @Override
    public synchronized void reset() throws IOException {
        bbuf.reset();
    }

    /** Check if we're looking at a Byte Order Mark.
     * @return the inferred encoding; or null if no BOM seen.
     */
    public String checkByteOrderMark() throws IOException {
        int b1 = read();
        int b2 = b1 < 0 ? -1 : read();
        int b3 = b2 < 0 ? -1 : read();
        int b4 = b3 < 0 ? -1 : read();
        if (b1 == 0xEF && b2 == 0xBB && b3 == 0xBF) {
             // UTF-8 Byte Order Mark // UTF-8 Byte Order Mark
            bbuf.position(3);
            return "UTF-8";
        } else if (b1 == 0xFF && b2 == 0xFE && b3 != 0) {
            // UTF-16LE Byte Order Mark
            bbuf.position(2);
            return "UTF-16LE";
        } else if (b1 == 0xFE && b2 == 0xFF && b3 != 0) {
            // UTF-16BE Byte Order Mark
            bbuf.position(2);
            return "UTF-16BE";
        } else {
            bbuf.position(0);
            return null;
        }
    }

}
