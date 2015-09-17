package gnu.kawa.io;
import java.io.*;
import gnu.text.*;
import java.nio.charset.Charset;

/** An OutPort that provides access to an underlying OutputStream.
 * This supports hybrid "ports", that are both textual and binary.
 * The default is to view a binary port as also supporting Latin-1 text,
 * in a mode similar to traditional single-byte Unix: any char greater
 * than '\xFF' is converted to '?', and others are copied straight through.
 * We also support other encoding from the "text side" to the "binary side".
 * Some care is used to convert text in the Writer-side buffers to
 * be flushed to the OutputStream-side before doing byte output,
 * to make sure bytes are interleaved sanely.
 * This is optimized for Latin-1 and UTF-8 to avoid any buffering
 * in the text-to-binary encoding (except if a non-BMP-character is seen).
 */

public class BinaryOutPort extends OutPort {
    OutputStream strm;

    public OutputStream getOutputStream() {
        flushBuffer();
        return strm;
    }

    /** Create a simple BinaryOutPort.
     * Only Latin-1 is supported for text output.
     */
    public BinaryOutPort(OutputStream strm, Path path) {
        this(strm, new OutputStreamWriterSimple(strm, false), path);
    }

    /** Create a BinaryOutPort given an OutputStream and a wrapping Writer.
     * The Writer should filter or convert text before sending encoded
     * bytes to the OutputStream; normally it would be an OutputStreamWriter
     * or an OutputStreamWriterSimple.
     */
    public BinaryOutPort(OutputStream strm, Writer out, Path path,
                         boolean printPretty, boolean autoflush) {
        super(out, printPretty, autoflush, path);
        this.strm = strm;
    }

    private BinaryOutPort(OutputStream strm, Writer out, Path path) {
        super(out, path);
        this.strm = strm;
    }

    /** Create a Writer that encodes its text to a given OutputStream.
     * Equivalent to creating a new OutputStreamWriter, but is optimized
     * to use OutputStreamWriterSimple if the converstion is UTF-8
     * or ISO_8859_1.
     * (The strm argument is required to be a MyBufferedOutputStream because:
     * in the case we use a OutputStreamWriterSimple, we should use a
     * buffer for performance; in the case we use a OutputStreamWriter,
     * we need to use MyBufferedOutputStream, so we can avoid flush when doing
     * flushBuffer.  For simplicity we always use MyBufferedOutputStream.)
     */
    private static Writer makeConvertWriter(MyBufferedOutputStream strm,
                                            Charset conv) {
        String cname = conv.name();
        boolean isUtf8 = "UTF-8".equals(cname);
        if (isUtf8 || "ISO_8859_1".equals(cname))
            return new OutputStreamWriterSimple(strm, isUtf8);
        else
            return new OutputStreamWriter(strm, conv);
    }

    public static BinaryOutPort makeStandardPort(OutputStream strm, String path) {
        MyBufferedOutputStream bufstrm = new MyBufferedOutputStream(strm);
        Writer wr = makeConvertWriter(bufstrm, Charset.defaultCharset());
        return new BinaryOutPort(bufstrm,
                                 new LogWriter(wr),
                                 Path.valueOf(path), true, true);
    }

    public static BinaryOutPort openFile(OutputStream strm, Path path,
                                         Charset conv) {
        MyBufferedOutputStream bufstrm = new MyBufferedOutputStream(strm);
        Writer wr = makeConvertWriter(bufstrm, conv);
        return new BinaryOutPort(bufstrm, wr, path);
    }

    public static BinaryOutPort openFile(Object fname)
        throws IOException {
        return (BinaryOutPort) OutPort.openFile(fname, Boolean.FALSE);
    }

    /** Kluge because OutputStreamWriter doen't have a public flushBuffer. */
    @Override
    void flushBuffer() {
        if (strm instanceof MyBufferedOutputStream) {
            MyBufferedOutputStream mstr = (MyBufferedOutputStream) strm;
            mstr.disableFlush(true);
            try {
                flush();
            } finally {
                mstr.disableFlush(false);
            }
        } else
            super.flushBuffer();
    }

    public void writeBytes(byte[] buf, int off, int len) throws IOException {
        flushBuffer();
        strm.write(buf, off, len);
    }

    public void writeByte(int b) throws IOException {
        flushBuffer();
        strm.write(b);
    }

    public static OutputStream asOutputStream(Object obj) {
        if (obj instanceof BinaryOutPort)
            return ((BinaryOutPort) obj).getOutputStream();
        else
            return (OutputStream) obj;
    }

    @Override
    public int getColumnNumber () { return -1; }

    /** This an extension of BufferedOutputStream for smarter flushing.
     * Specifically, before doing binary output, we need to flush whatever
     * is in the text buffer, but we don't want a full flush.
     */
    static class MyBufferedOutputStream extends BufferedOutputStream {
        public MyBufferedOutputStream(OutputStream out) {
            super(out);
        }

        boolean flushDisabled;
        public void disableFlush(boolean flushDisabled) {
            this.flushDisabled = flushDisabled;
        }

        @Override
        public void flush() throws IOException {
            if (! flushDisabled)
                super.flush();
        }
    }

    /** Like an OutputStreamWriter, but optimized for Latin1 or UTF-8.
     * This converter performs no buffering, which means interleaving
     * bytes and characters is more robust; therefore it is recommended
     * that the underlying OutputStream be buffered.
     */
    public static class OutputStreamWriterSimple extends Writer {
        OutputStream strm;
        boolean utf8;
        int pendingHighSurrogate;

        public OutputStreamWriterSimple(OutputStream strm, boolean utf8) {
            super(strm);
            this.strm = strm;
            this.utf8 = utf8;
        }

        private void write1(int ch) throws IOException {
            if (ch <= (utf8 ? 127 : 255))
                strm.write((int) ch);
            else if (! utf8)
                strm.write('?');
            else {
                int cont = 0;
                if (ch < 0x7FF) {
                    strm.write(0xC0 | (ch >> 6) & 0x1F);
                    cont = 1;
                } else if (ch >= 0xD800 && ch <= 0xDBFF) {
                    pendingHighSurrogate = ch;
                    return;
                } else if (ch >= 0xDC00 && ch <= 0xDFFF
                           && pendingHighSurrogate > 0) {
                    ch = (pendingHighSurrogate - 0xD800) * 0x400
                        + (ch - 0xDC00) + 0x10000;
                    strm.write(0xF0 | (ch >> 18) & 0x7);
                    cont = 3;
                    pendingHighSurrogate = 0;
                } else {
                    strm.write(0xE0 | (ch >> 12) & 0xF);
                    cont = 2;
                }
                while (--cont >= 0) {
                    strm.write(0x80 | ((ch >> (6 * cont)) & 0x3F));
                }
            }
        }

        @Override
        public void write(int ch) throws IOException {
            synchronized(lock) {
                write1(ch);
            }
        }
        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            synchronized (lock) {
                for (int i = 0;  i < len;  i++) {
                    char ch = cbuf[off+i];
                    write1(ch);
                }
            }
        }

        @Override
        public void write(String str, int off, int len)throws IOException {
            synchronized (lock) {
                for (int i = 0;  i < len;  i++) {
                    char ch = str.charAt(off+i);
                    write1(ch);
                }
            }
        }

        @Override
        public void flush()  throws IOException {
            synchronized (lock) {
                strm.flush();
            }
        }

        public void close()  throws IOException {
            synchronized (lock) {
                strm.close();
            }
        }
    }
}
