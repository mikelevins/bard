// Copyright (c) 2000, 2001, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer is something that will accept data (output),
 * and do something with it.
 * A consumer is like a SAX DocumentHandler or a PrintWriter,
 * but more abstract.  If a Sequence class impleemnts Consumer,
 * then data "written" to the sequence will be inserted in the sequence.
 * <p>
 * <em>Note:</em> This interface is not quite final.  For example it is
 * probable we will add methods for comments, processing instructions, etc.
 */

public interface Consumer
  /* #ifdef JAVA5 */
  extends Appendable
  /* #ifdef JAVA8 */
          , java.util.function.Consumer<Object>,
          java.util.function.IntConsumer,
          java.util.function.LongConsumer,
          java.util.function.DoubleConsumer
  /* #endif */
  /* #endif */
{
  public void writeBoolean(boolean v);

  public void writeFloat(float v);
  public void writeDouble(double v);
  public void writeInt(int v);
  public void writeLong(long v);

  public void startDocument();
  public void endDocument();

  public void startElement(Object type);
  public void endElement();

  /** Write a attribute for the current element.
   * This is only allowed immediately after a startElement. */
  public void startAttribute(Object attrType);

  /** End of an attribute or end of an actual parameter.
   * The former use matches a startAttribute; the latter may not,
   * and can be used to separate parameters in a parameter list.
   * This double duty suggsts the method should at least be re-named. */
  public void endAttribute();

  public void writeObject(Object v);

  /** True if consumer is ignoring rest of element.
   * The producer can use this information to skip ahead. */
  public boolean ignoring();

  public void write(int ch);
  public void write(String string);
  /* #ifdef use:java.lang.CharSequence */
  public void write(CharSequence string, int start, int length);
  /* #else */
  // public void write(String string, int start, int length);
  /* #endif */
  
  public void write(char[] buf, int start, int length);

  /* #ifdef JAVA5 */
  public Consumer append (char c);
  public Consumer append (CharSequence csq);
  public Consumer append (CharSequence csq, int start, int end);
  /* #endif */

  /* #ifdef JAVA8 */
  default public void accept(Object t) { writeObject(t); }
  default public void accept(int t) { writeInt(t); }
  default public void accept(long t) { writeLong(t); }
  default public void accept(double t) { writeDouble(t); }
  /* #endif */
}
