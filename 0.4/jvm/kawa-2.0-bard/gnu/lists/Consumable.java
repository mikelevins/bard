// Copyright (c) 2000, 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** An object that can send its contents to a Consumer. */

public interface Consumable
{
  public void consume(Consumer out);
}
