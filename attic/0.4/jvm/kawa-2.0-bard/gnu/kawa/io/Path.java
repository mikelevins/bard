// Copyright (c) 2007  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.kawa.io;
import java.io.*;
import java.net.*;
import gnu.mapping.*;
/* #ifdef JAVA7 */
import java.nio.file.Files;
/* #endif */
import java.nio.charset.Charset;
 
/** A generalized path/location, including File and URIs. */

public abstract class Path
/* #ifdef JAVA6 */
implements javax.tools.FileObject
/* #endif */
{
    /** This is equivalent to the System {@code "user.dir"} property.
     * It is the initial (default) value of pathLocalation.
     */
    public static final FilePath userDirPath =
        FilePath.valueOf(System.getProperty("user.dir"));

    /** This is the current default path.
     * The initial value matches the {@code "user.dir"} property.
     * If a relative path is made absolute, it is resolved
     * relative to the current path.
     */
    public final static ThreadLocal<Path> pathLocation
        = new InheritableThreadLocal<Path>() {
        @Override
        protected Path initialValue() { return userDirPath; }

        @Override
        public void set(Path path) {
            if (path == null)
                super.remove();
            else {
                if (! path.isAbsolute())
                    path = path.getAbsolute();
                super.set(path);
            }
        }
    };

    protected Path() {
    }

    public static Path currentPath() {
        return pathLocation.get();
    }

    public static void setCurrentPath(Path path) {
        pathLocation.set(path);
    }

    public static Path coerceToPathOrNull(Object path) {
        if (path instanceof Path)
            return (Path) path;
        if (path instanceof URL)
            return URLPath.valueOf((URL) path);
        /* #ifdef use:java.net.URI */
        if (path instanceof URI)
            return URIPath.valueOf((URI) path);
        /* #endif */
        if (path instanceof File)
            return FilePath.valueOf((File) path);
        String str;
        if (path instanceof CharSequence) // FIXME: || UntypedAtomic
            str = path.toString();
        else
            return null;
        if (Path.uriSchemeSpecified(str))
            return URIPath.valueOf(str);
        else
            return FilePath.valueOf(str);
    }

    public static Path valueOf(Object arg) {
        Path path = coerceToPathOrNull(arg);
        if (path == null)
            throw new WrongType((String) null, WrongType.ARG_CAST, arg, "path");
        return path;
    }

    public static URL toURL(String str) {
        try {
            if (! Path.uriSchemeSpecified(str)) {
                Path cur = currentPath();
                Path path = cur.resolve(str);
                if (path.isAbsolute())
                    return path.toURL();
                str = path.toString();
            }
            return new URL(str);
        } catch (Exception ex) {
            throw WrappedException.wrapIfNeeded(ex);
        }
    }

    /** Helper routine to get the scheme part of a URI.
     * The scheme part is "http:" or "file:" or "ftp:" most commonly.
     * This functions searches for the first ':' that doesn't follow a '/'.
     * @return The length of the scheme component, not counting the colon,
     * (or alternatively the index of the colon), or -1 if the is no scheme.
     */
    public static int uriSchemeLength(String uri) {
        int len = uri.length();
        for (int i = 0;  i < len;  i++) {
            char ch = uri.charAt(i);
            if (ch == ':')
                return i;
            if (i == 0 ? ! Character.isLetter(ch)
                : (! Character.isLetterOrDigit(ch)
                   && ch != '+' && ch != '-' && ch != '.'))
                return -1;
        }
        return -1;
    }

    /** Tests if a URL has a scheme.
     * For convenience, we treat a 1-character "scheme" as an
     * MS-DOS-style "drive letter" - i.e. not a scheme. */
    public static boolean uriSchemeSpecified(String name) {
        int ulen = uriSchemeLength(name);
        if (ulen == 1 && File.separatorChar == '\\') {
            char drive = name.charAt(0);
            return ! ((drive >= 'a' && drive <= 'z')
                      || (drive >= 'A' && drive <= 'Z'));
        }
        return ulen > 0;
    }

    public abstract boolean isAbsolute();

    /** Does this path name a directory?
     * The default implementation returns true only if the path ends
     * with '/' or the separatorChar.
     */
    public boolean isDirectory() {
        String str = toString(); 
        int len = str.length();
        if (len > 0) {
            char last = str.charAt(len - 1);
            if (last == '/' || last == File.separatorChar)
                return true;
        }
        return false;
    }

    /** Delete file - for compatibility with FileObject. */
    public boolean delete() {
        try {
            deleteFile();
            return true;
        } catch (Exception ex) {
            return false;
        }
    }

    /** Delete file. */
    public void deleteFile() throws IOException {
        throw new UnsupportedOperationException();
    }

    public boolean exists() {
        return getLastModified() != 0;
    }

    public abstract long getLastModified();

    public long getContentLength() {
        return -1;
    }
 
    public abstract String getScheme();

    public String getAuthority() {
        return null;
    }

    public String getUserInfo() {
        return null;
    }

    public String getHost() {
        return null;
    }

    public abstract String getPath();

    public Path getDirectory() {
        if (isDirectory())
            return this;
        else
            return resolve("");
    }

    public Path getParent() {
        return resolve(isDirectory() ? ".." : "");
    }

    public String getLast() {
        String p = getPath();
        if (p == null)
            return null;
        int len = p.length();
        int end = len;
        for (int i = len; ; ) {
            if (--i <= 0)
                return "";
            char c = p.charAt(i);
            if (c == '/'
                || (this instanceof FilePath
                    && c == File.separatorChar)) {
                if (i+1 == len)
                    end = i;
                else
                    return p.substring(i+1, end);
            }
        }
    }

    public String getExtension() {
        String p = getPath();
        if (p == null)
            return null;
        int len = p.length();
        for (int i = len; ; ) {
            if (--i <= 0)
                return null;
            char c = p.charAt(i);
            boolean sawDot = false;
            if (c == '.') {
                c = p.charAt(i-1);
                sawDot = true;
            }
            if (c == '/'
                || (this instanceof FilePath
                    && c == File.separatorChar))
                return null;
            if (sawDot)
                return p.substring(i+1);
        }
    }

    public int getPort() {
        return -1;
    }

    public String getQuery() {
        return null;
    }

    public String getFragment() {
        return null;
    }

    public abstract URL toURL();

    /* #ifdef use:java.net.URI */
    public abstract URI toUri();
    /* @deprecated */
    public final URI toURI() { return toUri(); }
    /* #else */
    // public String toUri() { return toURIString(); }
    // /* @deprecated */
    // public final String toURI() { return toUri(); }
    /* #endif */
    public String toURIString() { return toUri().toString(); }

    public Path resolve(Path relative) {
        if (relative.isAbsolute())
            return relative;
        return resolve(relative.toString());
    }

    public abstract Path resolve(String relative);

    public static InputStream openInputStream(Object uri) throws IOException {
        return Path.valueOf(uri).openInputStream();
    }

    public abstract InputStream openInputStream() throws IOException;
    public abstract OutputStream openOutputStream() throws IOException;

    public Reader openReader(boolean ignoreEncodingErrors) throws IOException {
        throw new UnsupportedOperationException(); // FIXME
    }

    public Writer openWriter() throws IOException {
        return new OutputStreamWriter(openOutputStream());
    }

    public CharSequence getCharContent(boolean ignoreEncodingErrors)
        throws IOException {
        byte[] bytes = readAllBytes();
        /* #ifdef JAVA6 */
        return new String(bytes, Charset.defaultCharset());
        /* #else */
        // return new String(bytes,
        //                   System.getProperty("sun.jnu.encoding", 
        //                                      "UTF-8"));
        /* #endif */
    }


    public byte[] readAllBytes() throws IOException {
        long len = getContentLength();
        // See comment for MAX_BUFFER_SIZE in java.nio.file.Files.
        if (len > Integer.MAX_VALUE - 8)
            throw new Error("path contents too big");
        int ilen = (int) len;
        byte[] buffer = new byte[ilen];
        int sofar = 0;
        InputStream in = openInputStream();
        while (sofar < ilen) {
            int cnt = in.read(buffer, sofar, ilen-sofar);
            if (cnt <= 0)
                throw new IOException("unable to read enture file");
            sofar += cnt;
        }
        return buffer;
    }

    /** Convert an absolute URI to one relatve to a given base.
     * This goes beyond java.net.URI.relativize in that if the arguments
     * have a common prefix, it can create a relative URI using "../" steps.
     */
    public static String relativize(String in, String base)
            throws java.net.URISyntaxException, java.io.IOException {
        String baseStr = base;
        String inStr = in;
        /* #ifdef use:java.net.URI */
        baseStr = new URI(baseStr).normalize().toString();
        inStr = URLPath.valueOf(in).toURI().normalize().toString();
        /* #endif */
        int baseLen = baseStr.length();
        int inLen = inStr.length();
        int i = 0;
        int sl = 0;
        int colon = 0;
        for (; i < baseLen && i < inLen;  i++) {
            char cb = baseStr.charAt(i);
            char ci = inStr.charAt(i);
            if (cb != ci)
                break;
            if (cb == '/')
                sl = i;
            if (cb == ':')
                colon = i;
        }
        if (colon > 0
            && (sl > colon + 2 || baseLen <= colon+2 || baseStr.charAt(colon+2) != '/')
            /*
            && (colon + 2 != CLASS_RESOURCE_URI_PREFIX_LENGTH
                || ! inStr.substring(0, colon + 2).equals(CLASS_RESOURCE_URI_PREFIX)
                || getClassLoaderForURI(base) == getClassLoaderForURI(in))
            */
            ) {
            baseStr = baseStr.substring(sl+1);
            inStr = inStr.substring(sl+1);
        }
        else
            return in;
        /* #ifdef JAVA5 */
        StringBuilder sbuf = new StringBuilder();
        /* #else */
        // StringBuffer sbuf = new StringBuffer();
        /* #endif */
        sl = 0;
        for (i = baseLen = baseStr.length(); --i >= 0; )
            if (baseStr.charAt(i) == '/') // sep?
                sbuf.append("../");
        sbuf.append(inStr);
        return sbuf.toString();
    }

    public String getName() {
        return toString();
    }

    public Path getAbsolute() {
        if (this == Path.userDirPath)
            return resolve("");
        else
            return currentPath().resolve(this);
    }

    public Path getCanonical() {
        return getAbsolute();
    }

    /* #ifdef JAVA7 */
    /** Convert if possible to a {@code java.nio.file.Path} instance.
     * Use caution if this is a relative path and the {@code currentPath()}
     * is not the default path, since {@code java.nio} assumes a relative path
     * is relative to the default directory.
     * Uses {@code java.nio.file.Paths#get(URI)}, and thus
     * throws whatever that method throws.
     * @throws FileSystemNotFoundException - no nio file system provider
     *   was found for the URI scheme
     */
    public java.nio.file.Path toNPath()
        throws java.nio.file.FileSystemNotFoundException {
        return java.nio.file.Paths.get(toUri());
    }
    /* #endif */

    public String probeContentType() {
        String contentType;
        /* #ifdef JAVA7 */
        try {
            contentType = Files.probeContentType(getAbsolute().toNPath());
        } catch (Exception ex) {
            contentType = null;
        }
        /* #else */
        // contentType = null;
        /* #endif */
        if (contentType == null) {
            contentType = URLConnection.guessContentTypeFromName(getPath());
        }
        return contentType;
    }

    /** Equivalent to the current value of {@code currentPath()}. */
    public static final Object PATH_CURRENT = new String("<current>");

    /** Resolve relative to the "current" or "parent" file. */
    public static final Object PATH_RELATIVE = new String("<relative>");

    /** Search for a readable file in a searchpath.
     * @param searchPath each element is a string, a Path (or convertible
     * to a Path using {@code Path.valueOf}) or the special values
     *   {@code PATH_CURRENT} or {@code PATH_RELATIVE}.
     * @param filename the name of the file to look for
     * @param base the base path for a PATH_RELATIVE path
     * @return either null (if no match as found), or a 2-element array
     *   {@code { inputStream, matchingPath }}.
     */
    public static Object[] search(Object[] searchPath, String filename,
                                  String base) {
        for (int i = 0;  i < searchPath.length;  i++) {
            Object spath = searchPath[i];
            if (spath == PATH_RELATIVE) {
                String tpath = base;
                if (tpath == null || tpath.startsWith("/dev/"))
                    spath = PATH_CURRENT;
                else
                    spath = tpath;
                }
            Path path;
            if (spath == PATH_CURRENT)
                path = Path.currentPath();
            else
                path = Path.valueOf(spath); // May throw exception
            try {
                InputStream istrm = path.resolve(filename).openInputStream();
                return new Object[] { istrm, path };
            } catch (Exception ex) {
            }
        }
        return null;
    }
}
