// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/**
 * A Sequence is an ordered list of elements.
 * It is similar to and compatible with the Java2 java.util.List interface,
 * but does not require it.
 *
 * All standard classes that implement Sequence also extend AbstractSequence.
 * Using AbstractSequence provides default implementations for many methods,
 * and also makes things a bit more efficient.  However, client code should
 * use Sequence rather than AbstractSequence.
 *
 * @author Per Bothner
 */

public interface Sequence<E>
extends
    /* #ifdef JAVA2 */
    java.util.List<E>,
    /* #endif */
    Consumable
{
  /** Special magic end-of-file marker. */
  public static final Object eofValue = EofClass.eofValue;

  /** True is this sequence contains no elements. */
  public boolean isEmpty();

  /** See java.util.List. */
  public int size();

  /** See java.util.List. */
  public E get (int index);

  /** See java.util.List. */
  public E set (int index, E value);

  public void fill(E value);

  public java.util.Enumeration<E> elements();

  /** Return code used to indicate a position is at end of the sequence. */
  public static final int EOF_VALUE = 0;
  public static final int PRIM_VALUE = 16;
  public static final int INT_U8_VALUE = PRIM_VALUE + 1;
  public static final int INT_S8_VALUE = PRIM_VALUE + 2;
  public static final int INT_U16_VALUE = PRIM_VALUE + 3;
  public static final int INT_S16_VALUE = PRIM_VALUE + 4;
  public static final int INT_U32_VALUE = PRIM_VALUE + 5;
  public static final int INT_S32_VALUE = PRIM_VALUE + 6;
  public static final int INT_U64_VALUE = PRIM_VALUE + 7;
  public static final int INT_S64_VALUE = PRIM_VALUE + 8;

  /** Return code used to indicate next element is 32-bit float. */
  public static final int FLOAT_VALUE = PRIM_VALUE + 9;

  /** Return code used to indicate next element is 64-bit double. */
  public static final int DOUBLE_VALUE = PRIM_VALUE + 10;
  public static final int BOOLEAN_VALUE = PRIM_VALUE + 11;
  /** A byte in an encoded string.
   * Part of a char, in contrast with INT_S8_VALUE, which is an integer. */
  public static final int TEXT_BYTE_VALUE = PRIM_VALUE + 12;
  public static final int CHAR_VALUE = PRIM_VALUE + 13;
  public static final int CDATA_VALUE = 31;
  public static final int OBJECT_VALUE = 32;
  public static final int ELEMENT_VALUE = 33;
  public static final int DOCUMENT_VALUE = 34;
  public static final int ATTRIBUTE_VALUE = 35;
  public static final int COMMENT_VALUE = 36;
  public static final int PROCESSING_INSTRUCTION_VALUE = 37;
  /*
  public static final int NAMESPACE_ATTRIBUTE_VALUE = 16;
  public static final int ENTITY_REFERENCE_VALUE = 16;
  */
}
