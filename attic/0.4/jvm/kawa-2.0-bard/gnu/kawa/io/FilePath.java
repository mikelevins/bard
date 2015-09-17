// Copyright (c) 2007, 2014  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.kawa.io;
import java.io.*;
import java.net.*;
import gnu.mapping.*;

/** A wrapper around a {@code java.io.File} that extends {@code Path}. */

public class FilePath
  extends Path
  /* #ifdef JAVA5 */
  implements Comparable<FilePath>
  /* #else */
  // implements Comparable
  /* #endif */
{
  final File file;
  /** Usually the same as {@code file.toString()}.
   * One important difference: {@code isDirectory} is true
   * if {@code path} ends with the {@code '/'} or the {@code separatorChar}.
   * The original String if constructed from a String.
   */
  final String path;

  private FilePath (File file)
  {
    this.file = file;
    this.path = file.toString();
  }

  private FilePath (File file, String path)
  {
    this.file = file;
    this.path = path;
  }

  public static FilePath valueOf (String str)
  {
    String orig = str;
    /* FIXME: Should we expand '~'?
       Issues: is (path "~/bar") absolute?
       What about: (base:resolve "~/bar") ?
       What if base above isn't a FilePath?
    int len = str.length();
    if (len > 0 && str.charAt(0) == '~' && File.separatorChar == '/')
      {
        if (len == 1 || str.charAt(1) == '/')
          {
            String user = System.getProperty("user.home");
            if (user != null)
              str = user + str.substring(1);
          }
        else
          {
            // We don't support '~USER/...'  Do that using /bin/sh. FIXME
          }
      }
    */
    return new FilePath(new File(str), orig);
  }

    public static FilePath valueOf(File file) {
        return new FilePath(file);
    }

    public static FilePath valueOf(URI uri) {
        if (uri.isAbsolute())
            return FilePath.valueOf(new File(uri));
        String ustr = uri.toString();
        char sep = File.separatorChar;
        if (sep != '/')
            ustr = ustr.replace('/', sep);
        return new FilePath(new File(ustr));
    }

    public static FilePath coerceToFilePathOrNull(Object path) {
        if (path instanceof FilePath)
            return (FilePath) path;
        if (path instanceof URIPath)
            path = ((URIPath) path).uri;
        if (path instanceof URI)
            return FilePath.valueOf((URI) path);
        if (path instanceof File)
            return FilePath.valueOf((File) path);
        String str;
        if (path instanceof CharSequence) // FIXME: || UntypedAtomic
            str = path.toString();
        else
            return null;
        return FilePath.valueOf(str);
    }

    public static FilePath makeFilePath(Object arg) {
        FilePath path = coerceToFilePathOrNull(arg);
        if (path == null)
            throw new WrongType((String) null, WrongType.ARG_CAST,
                                arg, "filepath");
        return path;
    }

    public boolean isAbsolute() {
        return this == Path.userDirPath || file.isAbsolute();
    }

    /** Does this path represent a directory?
     * It is assumed to be a directory if the path ends with '/'
     * or File.separatorChar - or if the file exists and is a directory.
     */
    @Override
    public boolean isDirectory() {
        int len = path.length();
        if (len > 0) {
            char last = path.charAt(len - 1);
            if (last == '/' || last == File.separatorChar)
                return true;
        }
        return toFile().isDirectory();
    }

    @Override
    public void deleteFile() throws IOException {
        /* #ifdef JAVA7 */
        java.nio.file.Files.delete(toNPath());
        /* #else */
        // if (! toFile().delete())
        //     throw new IOException("cannot delete - "+this);
        /* #endif */
    }

  public long getLastModified ()
  {
    return toFile().lastModified();
  }

  public boolean exists ()
  {
    return toFile().exists();
  }

  public long getContentLength ()
  {
    File f = toFile();
    long length = f.length();
    return length == 0 && ! f.exists() ? -1 : length;
  }
 
  public String getPath ()
  {
    return file.getPath();
  }

  public String getLast ()
  {
    return file.getName();
  }

  public
  /* #ifdef JAVA5 */
  FilePath
  /* #else */
  // Path
  /* #endif */
  getParent ()
  {
    File parent = file.getParentFile();
    if (parent == null)
      return null;
    else
      return FilePath.valueOf(parent);
  }

  public int compareTo (FilePath path)
  {
    return file.compareTo(path.file);
  }

  /* #ifndef JAVA5 */
  // public int compareTo (Object obj)
  // {
  //   return compareTo((FilePath) obj);
  // }
  /* #endif */

  public boolean equals (Object obj)
  {
    return obj instanceof FilePath && file.equals(((FilePath) obj).file);
  }

  public int hashCode ()
  {
    return file.hashCode();
  }

  public String toString ()
  {
    return path;
  }

  public File toFileRaw ()
  {
    return file;
  }

  public File toFile() {
      if (file.isAbsolute())
          return file;
      Path cur = currentPath();
      if (cur == userDirPath)
          return file;
      return ((FilePath) cur.resolve(this)).toFileRaw();
  }

  public URL toURL ()
  {
    if (this == Path.userDirPath)
      return resolve("").toURL();
    if (! isAbsolute())
      return getAbsolute().toURL();
    try
      {
        return file.toURI().toURL();
      }
    catch (Exception ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
  }

  public URI toUri() {
      try {
          if (file.isAbsolute())
              return file.toURI();
          /* We don't want to just use File.toURI(),
             because that turns a relative File into an absolute URI. */
          String fname = path;
          char fileSep = File.separatorChar;
          if (fileSep != '/')
              fname = fname.replace(fileSep, '/');
          int len = fname.length();
          if (len > 0 && fname.charAt(len-1) != '/'
              && isDirectory())
              fname = fname + '/';
          return new URI(null, null, fname, null);
      } catch (Exception ex) {
          throw WrappedException.wrapIfNeeded(ex);
      }
  }

    public InputStream openInputStream() throws IOException {
        return new FileInputStream(toFile());
    }

    public OutputStream openOutputStream() throws IOException {
        return new FileOutputStream(toFile());
    }

    public OutputStream openAppendStream() throws IOException {
        return new FileOutputStream(toFile(), true);
    }

    public String getScheme() {
        return isAbsolute() ? "file" : null;
    }

    public Path resolve(String relative) {
        if (Path.uriSchemeSpecified(relative))
            return URLPath.valueOf(relative);
        return valueOf(toUri().resolve(relative));
    }

  public Path getCanonical ()
  {
    try
      {
        File canon = file.getCanonicalFile();
        if (! canon.equals(file))
          return valueOf(canon);
      }
    catch (Exception ex)
      {
      }
    return this;
  }

    /* #ifdef JAVA7 */
    /** Convert to a {@code java.nio.file.Path} instance.
     * Unlike the overriden base method, this cannot
     * throw {@code FileSystemNotFoundException}.
     * Use caution if this is a relative path and the {@code currentPath()}
     * is not the default path, since {@code java.nio} assumes a relative path
     * is relative to the default directory.
     */
    @Override
    public java.nio.file.Path toNPath() {
        return file.toPath();
    }
    @Override
    public byte[] readAllBytes() throws IOException {
        Path rpath = this;
        if (! isAbsolute()) {
            Path cur = currentPath();
            if (cur != Path.userDirPath)
                rpath = cur.resolve(this);
        }
        return java.nio.file.Files.readAllBytes(rpath.toNPath());
    }
    /* #endif */
}
