package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.kawa.functions.NumberCompare;
import gnu.kawa.xml.KNode;
import gnu.kawa.xml.UntypedAtomic;

/** Helper class used in conjunction with {@link OrderedMap}.
 * It has the tuples from the {@code for} and {@code let}-clauses,
 * as filtered by the {@code where}-clause.
 *
 * The tuples are sorted using a linked-list version of merge sort.
 *
 * The sequence of n tuples for m variables is represented using
 * an array of length n where each element is an array of length m.
 * A possible future optimization would be to instead use m
 * different arrays of of length n.  The advantage is that each
 * of the M arrays could have the "correct" type for each variable,
 * and so we avoid casts or boxing/unboxing.
 */

public class OrderedTuples extends FilterConsumer
{
  /** The number of tuples. */
  int n;

  /** The sequence of tuples, in input (unsorted) order. */
  Object[] tuples; // Actually: Object[][] tuples.

  /** The compator functions.
   * If there are k comparator, the array's length is 3*k.
   * comps[3*i] is the i'th comparison function
   *   (represented as a procedure on a tuple);
   * comps[3*i+1] is the i'th set of flags encoded as a string;
   * and comps[3*i+2] is the i'th collator
   *   (either null or a NamedCollator).
   */
  Object[] comps;

  /* The index of the first tuple, after sorting. */
  int first;
  /** Used to chain the tuples after sorting.
   * I.e. if the i'th tuple (is sort order) is tuples[k],
   * then the (i+1)'the sorted tuple is tuples[next[k]].
   * The end of the list is indicated by -1.
   */
  int[] next;

  /** The return clause, encoded as a procedure on a tuple. */
  Procedure body;

    public boolean ignoring() { return false; }

  public void writeObject(Object v)
  {
    if (n >= tuples.length)
      {
        Object[] tmp = new Object[2 * n];
        System.arraycopy(tuples, 0, tmp, 0, n);
        tuples = tmp;
      }
    tuples[n++] = v;
  }

  OrderedTuples ()
  {
    super(null);
    tuples = new Object[10];
  }

  public static OrderedTuples make$V (Procedure body, Object[] comps)
  {
    OrderedTuples tuples = new OrderedTuples();
    tuples.comps = comps;
    tuples.body = body;
    return tuples;
  }

  public void run$X (CallContext ctx)  throws Throwable
  {
    first = listsort(0);
    emit(ctx);
  }

  void emit (CallContext ctx)  throws Throwable
  {
    for (int p = first; p >= 0; p = next[p])
      emit(p, ctx);
  }

  void emit (int index, CallContext ctx)  throws Throwable
  {
    Object[] args = (Object[]) tuples[index];
    body.checkN(args, ctx);
    ctx.runUntilDone();
  }

  // The following sort routine is derived from Simon Tatham's listsort.c.
  // However we use array indexes instead of pointers, and the next
  // element instead of a next field.
  // I.e. p->next is mapped to next[p].
  // Instead of NULL we use -1.

  /*
   * Demonstration code for sorting a linked list.
   * 
   * The algorithm used is Mergesort, because that works really well
   * on linked lists, without requiring the O(N) extra space it needs
   * when you do it on arrays.
   */

/*
 * This file is copyright 2001 Simon Tatham.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL SIMON TATHAM BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

int cmp(int a, int b)  throws Throwable
{
  for (int i = 0;  i < comps.length;  i += 3)
    {
      Procedure comparator = (Procedure) comps[i];
      String flags = (String) comps[i+1];
      NamedCollator collator = (NamedCollator) comps[i+2];
      if (collator == null)
        collator = NamedCollator.codepointCollation;
      Object val1 = comparator.applyN((Object[]) tuples[a]);
      Object val2 = comparator.applyN((Object[]) tuples[b]);
      val1 = KNode.atomicValue(val1);
      val2 = KNode.atomicValue(val2);
      if (val1 instanceof UntypedAtomic)
        val1 = val1.toString();
      if (val2 instanceof UntypedAtomic)
        val2 = val2.toString();
      boolean empty1 = SequenceUtils.isEmptySequence(val1);
      boolean empty2 = SequenceUtils.isEmptySequence(val2);
      if (empty1 && empty2)
        continue;
      int c;
      if (empty1 || empty2)
        {
          char emptyOrder = flags.charAt(1);
          c = empty1 == (emptyOrder == 'L') ? -1 : 1;
        }
      else
        {
          boolean isNaN1 = val1 instanceof Number
            && Double.isNaN(((Number) val1).doubleValue());
          boolean isNaN2 = val2 instanceof Number
            && Double.isNaN(((Number) val2).doubleValue());
          if (isNaN1 && isNaN2)
            continue;
          if (isNaN1 || isNaN2)
            {
              char emptyOrder = flags.charAt(1);
              c = isNaN1 == (emptyOrder == 'L') ? -1 : 1;
            }
          else if (val1 instanceof Number && val2 instanceof Number)
            c = NumberCompare.compare(val1, val2, false);
          else
            c = collator.compare(val1.toString(), val2.toString());
        }
      if (c == 0)
        continue;
      return flags.charAt(0) == 'A' ? c : -c;
    }
  return 0;
}

/*
 * This is the actual sort function. Notice that it returns the new
 * head of the list. (It has to, because the head will not
 * generally be the same element after the sort.) So unlike sorting
 * an array, where you can do
 * 
 *     sort(myarray);
 * 
 * you now have to do
 * 
 *     list = listsort(mylist);
 */
  int listsort(int list) throws Throwable
  {// indexes
    int p, q, e, tail; 
    int insize, nmerges, psize, qsize, i;

    /*
     * Silly special case: if `list' was passed in as NULL, return
     * NULL immediately.
     */
    if (n == 0)
	return -1;

    next = new int[n];

    for (i = 1; ;  i++)
      {
        if (i == n)
          {
            next[i-1] = -1;
            break;
          }
        else
          next[i-1] = i;
      }

    insize = 1;

    for (;;) {
        p = list;
        list = -1;
        tail = -1;

        nmerges = 0;  /* count number of merges we do in this pass */

        while (p >= 0) {
            nmerges++;  /* there exists a merge to be done */
            /* step `insize' places along from p */
            q = p;
            psize = 0;
            for (i = 0; i < insize; i++) {
                psize++;
                q = next[q];
                if (q < 0) break;
            }
            /* if q hasn't fallen off end, we have two lists to merge */
            qsize = insize;

            /* now we have two lists; merge them */
            while (psize > 0 || (qsize > 0 && q >= 0)) {

                /* decide whether next element of merge comes from p or q */
                if (psize == 0) {
		    /* p is empty; e must come from q. */
		    e = q; q = next[q]; qsize--;
		} else if (qsize == 0 || q < 0) {
		    /* q is empty; e must come from p. */
		    e = p; p = next[p]; psize--;
		} else if (cmp(p,q) <= 0) {
		    /* First element of p is lower (or same);
		     * e must come from p. */
		    e = p; p = next[p]; psize--;
		} else {
		    /* First element of q is lower; e must come from q. */
		    e = q; q = next[q]; qsize--;
		}

                /* add the next element to the merged list */
		if (tail >= 0)
                  next[tail] = e;
		else
		    list = e;
		tail = e;
            }

            /* now p has stepped `insize' places along, and q has too */
            p = q;
        }
        next[tail] = -1;

        /* If we have done only one merge, we're finished. */
        if (nmerges <= 1)   /* allow for nmerges==0, the empty list case */
            return list;

        /* Otherwise repeat, merging lists twice the size */
        insize *= 2;
    }
}
}
