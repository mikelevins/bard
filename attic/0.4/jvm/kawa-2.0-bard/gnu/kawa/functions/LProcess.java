package gnu.kawa.functions;
import gnu.mapping.Lazy;
import gnu.mapping.WrappedException;
import gnu.lists.Blob;
import java.io.InputStream;
import java.io.OutputStream;

/** A process whose lazy "value" is the bytes from standard output.
 */

public class LProcess extends Process
    implements Lazy<Blob>
 {
    private Process base;
    private Blob value;
    private Throwable exception;

    public LProcess(Process process) {
        this.base = process;
    }

    @Override
    public InputStream getInputStream() {
        return getValue().getInputStream();
    }

    @Override
    public OutputStream getOutputStream() {
        return base.getOutputStream();
    }

    @Override
    public InputStream getErrorStream() {
        return base.getErrorStream();
    }

    @Override
    public int waitFor() throws InterruptedException {
        return base.waitFor();
    }

    @Override
    public int exitValue() {
        return base.exitValue();
    }

    @Override
    public void destroy() {
        base.destroy();
    }

    public Blob getValue() {
        synchronized (this) {
            if (value == null) {
                try {
                    byte[] bytes = new byte[1024];
                    int len = 0;
                    InputStream in = base.getInputStream();
                    for(;;) {
                        int avail = bytes.length-len;
                        if (avail < 512) {
                            byte[] tmp = new byte[2*bytes.length];
                            System.arraycopy(bytes, 0, tmp, 0, len);
                            bytes = tmp;
                            avail = bytes.length-len;
                        }
                        int cnt = in.read(bytes, len, avail);
                        if (cnt < 0)
                            break;
                        len += cnt;
                    }
                    // If array is less than 75% full, reallocate.
                    if (4 * len < 3 * bytes.length) {
                        byte[] tmp = new byte[len];
                        System.arraycopy(bytes, 0, tmp, 0, len);
                        bytes = tmp;
                    }
                    value = Blob.wrap(bytes, len);
                } catch (Throwable ex) {
                    exception = ex;
                }
            
            }
            if (exception != null)
                WrappedException.rethrow(exception);
        }
        return value;
    }
}
