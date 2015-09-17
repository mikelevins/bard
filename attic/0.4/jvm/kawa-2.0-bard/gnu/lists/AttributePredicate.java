// Copyright (c) 2002  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A predicate that (only) matches a ATTRIBUTE_VALUE.
 * If using XML terminology:  only matches attribute nodes.
 */

public interface AttributePredicate extends NodePredicate
{
  public boolean isInstance(AbstractSequence seq, int ipos,
			    Object attributeType);
}
