package gnu.text;

public interface SourceLocator
  /* #ifdef SAX2 */
  extends
  /* #ifdef use:javax.xml.transform */
  javax.xml.transform.SourceLocator, 
  /* #endif */
  org.xml.sax.Locator
  /* #endif */
{
  /** Return current column number.
   * The "first" column is column 1; unknown is -1. */
  public int getColumnNumber();

  /** Return current line number.
   * The "first" line is line 1; unknown is -1. */
  public int getLineNumber();

  public String getPublicId();

  public String getSystemId();
  /** Normally same as getSystemId. */
  public String getFileName();

  /** True if position is unlikely to change.
   * True for an expression but not an input file. */
  public boolean isStableSourceLocation();
}
