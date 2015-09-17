// Copyright (c) 2013  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;
import java.util.*;
import gnu.math.*;

/** Helper class used in can-finish (termination) analysis.
 *
 * We have a set of execution paths (in general one path
 * for each fork/just seen, typically from each IfExp branch).
 * Each execution path has a set of LambdaExps whose termination we
 * depend on: We can reach this point along an execution path
 * if each LambdaExp in the set can finish.

 * The data structure is conceptually a map from LambdaExp to
 * a set of executions path that depend on the LambdaExp.
 * Since each execution path has an integer index, we can represent the set
 * as a bitstring, where all bitstrings have the same length (numPaths).
 * For simplicity and performance, we use IntNum for the bitstrings.
 */

class CanFinishMap {
    /** Number of different execution paths to this point. */
    int numPaths;

    /** For each function, the set to execution paths that depend on it.
     * I.e. for an execution path i, and a lambda lexp,
     * the current location depends on lexp completing normally
     * if pathMap(lexp) has bit i set.
     */
    Map<LambdaExp,IntNum> pathMap;

    /* A cache of {@code (1 << numPaths)-1}. */
    IntNum mask;

    private CanFinishMap(Map<LambdaExp,IntNum> map, int num, IntNum mask) {
        this.pathMap = map;
        this.numPaths = num;
        this.mask = mask;
    }

    /** Set numPaths, and also update the mask. */
    private void setNumPaths(int numPaths) {
        this.numPaths = numPaths;
        this.mask = IntNum.sub(IntNum.shift(IntNum.one(), numPaths),
                               IntNum.one());
    }

    /** This value is used to mark a function we know cannot terminate.
     * For example each execution path throws an exception or calls itself.
     */
    public static final CanFinishMap CANNOT_FINISH
        = new CanFinishMap(new HashMap<LambdaExp,IntNum>(), 0, IntNum.zero());

    /** This value is used to mark a function we know can terminate.
     * Of course this doesn't guarantee termination, just that there is
     * path through the execution path tree that can terminate,
     * according to this conservative analysis.
     */
    public static final CanFinishMap CAN_FINISH
        = new CanFinishMap(new HashMap<LambdaExp,IntNum>(), 1, IntNum.one());

    public CanFinishMap clone() {
        return new CanFinishMap(new HashMap<LambdaExp,IntNum>(pathMap),
                                numPaths, mask);
    }
                                
    /** Note that the currently active execution depend on {@code callee}.
     */
    public boolean addDependency(LambdaExp callee) {
        IntNum old = pathMap.get(callee);
        if (old != null && IntNum.equals(old, mask))
            return false;
        pathMap.put(callee, mask);
        return true;
    }

    /** "Join" paths from {@code other} to this.
     */
    public void addPaths(CanFinishMap other) {
        for (LambdaExp lexp : other.pathMap.keySet()) {
            IntNum otherMask = IntNum.shift(other.pathMap.get(lexp), numPaths);
            IntNum old = pathMap.get(lexp);
            pathMap.put(lexp,
                        old == null ? otherMask : BitOps.ior(old, otherMask));
        }
        setNumPaths(numPaths + other.numPaths);
    }

    /** Check if there is an execution path that can finish. */
    public boolean canFinish() {
        if (this == CANNOT_FINISH)
            return false;
        IntNum finishDeps = IntNum.zero();
        for (Iterator<LambdaExp> it = pathMap.keySet().iterator();
             it.hasNext(); ) {
            LambdaExp lexp = it.next();
            if (lexp.canFinishCondition == CanFinishMap.CAN_FINISH)
                it.remove();
            else
                finishDeps = BitOps.ior(finishDeps, pathMap.get(lexp));
        }
        // Return true if some significant bit in finishDeps is zero.
        return BitOps.lowestBitSet(BitOps.not(finishDeps)) < numPaths;
    }
}
