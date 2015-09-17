// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/**
 * An object that can be "fed" a TreePosition, and will do something with it.
 * That "something" can be printing it, copy it, filter it - or ignore it.
 */

public interface PositionConsumer
// FIXME rename to PositionList?  Then what about TreePositionList
{
  /**
   * Consume node at current position.
   *
   * The caller may invalidate or change the position after consume returns,
   * so if the consumer wants to save it, it needs to copy it.
   */
  public void writePosition(SeqPosition position);

  /** Consume a single position pair.
   * This PositionConsumer may assume the sequence does no reference
   * management; i.e. that copyPos is trivial and releasePos is
   * a no-op.  If that is not the case, use consume(TreePosition) instead.
   */
  public void writePosition(AbstractSequence seq, int ipos);

}
