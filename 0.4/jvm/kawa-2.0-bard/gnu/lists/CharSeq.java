// Copyright (c) 2001, 2005  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A sequence where each element is a character.
 */

public interface CharSeq
  extends
  /* #ifdef use:java.lang.CharSequence */
  CharSequence, 
  /* #endif */
  Sequence
{
  /** Get length of string, in characters.
   * Synonym for size(), for compatibility with String and StringBuffer. */
  public int length();

  public char charAt(int index);

  /** Copy characters into a destination buffer.
   * Same interface as java.lang.String's getChars. */
  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin);

  public void setCharAt(int index, char ch);
  public void setCharacterAt(int index, int ch);

  /** Set all the elements to a given character. */
  public void fill(char value);

  public void fill(int fromIndex, int toIndex, char value);

  /* #ifdef use:java.lang.CharSequence */
  public CharSequence subSequence(int start, int end);
  /* #endif */

  /* #ifdef JAVA5 */
  /** Append a specified subsequence to an <code>Appendable</code>.
   * An allowable implementation is:
   * <code>dest.append(this, start, start+count)</code>.
   * Hence implementors of <code>Appendable</code> should avoid calling
   * <code>writeTo</code> - though they can call <code>getChars</code>.
   */
  public void writeTo(int start, int count, Appendable dest)
    throws java.io.IOException;

  public void writeTo(Appendable dest)
    throws java.io.IOException;
  /* #else */
  // /**
  //  * Write out (part of) this string.
  //  * @param start index of initial character to write
  //  * @param count number of characters to write
  //  * @param dest where to write the characters
  //  */
  // public void writeTo(int start, int count, java.io.Writer dest)
  //   throws java.io.IOException;

  // public void writeTo(java.io.Writer str) throws java.io.IOException;
  /* #endif */

  public void consume(int start, int count, Consumer out);

  public String toString();
}
