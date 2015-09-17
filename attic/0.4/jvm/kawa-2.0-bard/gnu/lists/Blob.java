package gnu.lists;
import gnu.kawa.io.BinaryInPort;
import java.nio.charset.Charset;

/** Binary data which may represent text or other information.
 * It can be used for the contents of a Unix-style file or pipe,
 * which is commonly but not always text, so whether it should be
 * treated as bytes, text, or something else is context-dependent.
 * (Nowadays you'd want an associated MIME-type, but that is
 * not always provided.)
 * The acronym "blob" has been taken as "binary large object", but
 * in this case it isn't necessary large.
 */

public class Blob
    extends U8Vector
    implements Consumable
               , CharSequence
{
    private String stringValue;
    Charset charset;

    public Blob(byte[] data) {
        super(data);
    }

    public Blob(byte[] data, Charset charset) {
        super(data);
        this.charset = charset;
    }

    public static Blob wrap(byte[] data, int size) {
        Blob blob = new Blob(data);
        blob.size = size;
        return blob;
    }

    public U8Vector asPlainBytevector() {
        U8Vector vec = new U8Vector(data);
        vec.size = size;
        return vec;
    }

    public void consume(Consumer out) {
        // FIXME Maybe just writeObject(this)
        // and then have the consumer decide?
        out.write(toString());
    }

    public String toString() {
        synchronized (this) {
            if (stringValue == null) {
                BinaryInPort in = new BinaryInPort(data, size, null);
                // Caching the string value may not be a good idea.
                // Especially if we're just printing it.
                StringBuilder buf = new StringBuilder();
                try {
                    boolean bomSeen = false;
                    if (charset != null)
                        bomSeen = in.setFromByteOrderMark();
                    if (! bomSeen)
                        // FIXME should do some sniffing to guess encoding.
                        in.setCharset(charset != null ? charset
                                      : Charset.defaultCharset());
                    int ch;
                    while ((ch = in.read()) >= 0) {
                        buf.append((char) ch);
                    }
                } catch (Exception ex) {
                    buf.append("[unexpected exception: ");
                    buf.append(ex);
                    buf.append(']');
                }
                stringValue = buf.toString();
            }
            return stringValue;
        }
    }

    public char charAt(int index) { return toString().charAt(index); }
    public int length() { return toString().length(); }
    public CharSequence subSequence(int start, int end) {
        return toString().subSequence(start, end);
    }
}
