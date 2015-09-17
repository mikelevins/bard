// Copyright (c) 2002  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A predicate (or type) on an item in a sequence.
 */

public interface ItemPredicate
{
  public boolean isInstancePos (AbstractSequence seq, int ipos);
}
