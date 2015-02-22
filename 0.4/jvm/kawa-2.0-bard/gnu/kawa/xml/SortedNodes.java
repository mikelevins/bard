// Copyright (c) 2003, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;

/** Manages a sequence of node references in document order without duplicates.
 * The most recently added element is just before the gap.
 * Optimized for the data being in order, or at least having good
 * locality (a node being "near" the previously-entered node).
 */

public class SortedNodes extends Nodes {

    private int compareIndex(int index, AbstractSequence seq2, int ipos2) {
        if (index >= vector.gapStart)
            index += vector.gapEnd - vector.gapStart;
        AbstractSequence seq1 = nvector.getSeq(index);
        int ipos1 = nvector.getPos(index);
        return AbstractSequence.compare(seq1, ipos1, seq2, ipos2);
    }

    /** Find index where to put position (seq, ipos).
     * Require {@code index>=start && index<end},
     * where {@code end==start+count}.
     * Require all position before index are "less than" (seq, ipos),
     * and all positions after are "greater than" (seq, ipos).
     * If there is no such index (because it is "same as"), return -1.
     */
    private int find (int start, int count, AbstractSequence seq, int ipos) {
        int lo = 0;
        int hi = count;
        // We use binary search, though the arraycopy operations in
        // writePosition limit the benefit - a sequence of writePosition
        // calls is still quadratic in the worst case
        // (but linear if locality is good).
        while (lo < hi) {
            int mid = (lo + hi) >>> 1;
            int cmp = compareIndex(start + mid, seq, ipos);
            if (cmp == 0)
                return -1;
            if (cmp > 0)
                hi = mid;
            else
                lo = mid + 1;
        }
        return start + lo;
    }

    int find(AbstractSequence seq, int ipos) {
        int count = size();
        if (count <= 0)
            return 0;
        // Optimize for the already sorted case, or secondarily for good
        // locality.  So use the gapStart as the initial "mid-point".
        int lastIndex = vector.gapStart - 1;
        int cmp = lastIndex < 0 ? -1 : compareIndex(lastIndex, seq, ipos);
        if (cmp < 0) {
            // The new node is after all nodes up to gapStart.
            int i = lastIndex+1;
            // Note that if the incoming nodes are already sorted (a
            // common case in path expressions), then find will
            // immediately return i.
            return find (i, count-i, seq, ipos);
        } else if (cmp == 0)
            return -1;
        else {
            return find (0, lastIndex, seq, ipos);
        }
    }

    @Override
    public void writePosition(SeqPosition position) {
        AbstractSequence seq = position.sequence;
        int ipos = position.ipos;
        int i = find(seq, ipos);
        if (i >= 0)
            vector.add(i, position);
    }

    @Override
    public void writePosition(AbstractSequence seq, int ipos) {
        int i = find(seq, ipos);
        if (i >= 0) {
            vector.add(i, (SeqPosition) null);
            nvector.setBuffer(i, seq, ipos);
        }
    }
}
