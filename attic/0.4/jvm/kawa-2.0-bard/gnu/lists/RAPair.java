/** SRFI-101 Purely Functional Random-Access Pairs and Lists.
 * Copyright Per Bothner 2013.
 * Copyright (c) David Van Horn 2009.  All Rights Reserved.
 * Adaptation/translation into Java of low-level core parts
 * of Scheme reference implementation.
 */

package gnu.lists;

public class RAPair extends ImmutablePair {
    public int size;

    public Object getTree() { return car; }
    public Object getRest() { return cdr; }

    public RAPair(int size, Object tree, Object rest) {
        super(tree, rest);
        this.size = size;
    }

    private static int half(int n) { return n >> 1; }

    private static Object treeVal(Object t) {
        return t instanceof Node ? ((Node) t).val : t;
    }

    public Object getCar() {
        if (car instanceof Node)
            return treeVal(car);
        else
            return car;
    }

    public Object getCdr() {
        if (car instanceof Node) {
            Node tnode = (Node) car;
            int sz = half(size);
            return new RAPair(sz,
                              tnode.left,
                              new RAPair(sz, tnode.right, cdr));
        }
        else
            return cdr;
    }

    public static Object treeRef(int size, Object t, int i) {
        if (i == 0)
            return treeVal(t);
        else
            return treeRefA(t, i, half(size-1));
    }

    // Special-cased above to avoid logarathmic amount of cons'ing
    // and any multi-values overhead.  Operates in constant space.
    // [Tree X] Nat Nat -> X
    // invariant: (= mid (half (sub1 (tree-count t))))
    public static Object treeRefA(Object t, int i, int mid)  {
        for (;;) {
            if (i == 0)
                return treeVal(t);
            else if (i <= mid) {
                t = ((Node) t).left;
                i--;
            } else {
                t = ((Node) t).right;
                i = i - mid - 1;
            }
            mid = half(mid-1);
        }
    }

    public static Object listRef(RAPair ls, int i) {
        RAPair xs = ls;
        int j = i;
        for (;;) {
            int xsize = xs.size;
            if (j < xsize)
                return treeRef(xsize, xs.car, j);
            j -= xsize;
            xs = (RAPair) xs.cdr;
        }
    }

    public Object get(int i) {
        return listRef(this, i);
    }

    public static RAPair cons(Object x, Object ls) {
        if (ls instanceof RAPair) {
            RAPair lspair = (RAPair) ls;
            int s = lspair.size;
            if (lspair.cdr instanceof RAPair) {
                RAPair lsrest = (RAPair) lspair.cdr;
                if (lsrest.size == s)
                    return new RAPair(s+s+1,
                                      new Node(x, lspair.car,
                                               lsrest.car),
                                      lsrest.cdr);
            }
        }
        return new RAPair(1, x, ls);
    }

    public static LList raList(Object[] xs) {
        LList result = LList.Empty;
        for (int i = xs.length; --i >= 0; )
            result = cons(xs[i], result);
        return result;
    }

    public static int raLength(Object ls) {
        int sz = 0;
        while (ls instanceof RAPair) {
            RAPair p = (RAPair) ls;
            sz += p.size;
            ls = p.cdr;
        }
        return sz;
    }

    public int size() {
        return raLength(this);
    }

    public static class Node {
        public Object val;
        public Object left;
        public Object right;

        public Node(Object val, Object left, Object right) {
            this.val = val;
            this.left = left;
            this.right = right;
        }
    }
}
