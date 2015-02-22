// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A Consumer that does nothing. */

public class VoidConsumer extends FilterConsumer
{
  public static VoidConsumer instance = new VoidConsumer();

  public static VoidConsumer getInstance() { return instance; }

  public VoidConsumer()
  {
    super(null);
    skipping = true;
  }

  /** True if consumer is ignoring rest of element.
   * The producer can use this information to skip ahead. */
  public boolean ignoring()
  {
    return true;
  }
}
