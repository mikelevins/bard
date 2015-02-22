// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

public class GeneralArray1 extends GeneralArray implements Sequence
{
  protected int nextIndex(int ipos)
  {
    return ipos == -1 ? size() : ipos >>> 1;
  }

  public void consumePosRange(int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int it = iposStart;
    while (! equals(it, iposEnd))
      {
	if (! hasNext(it))
	  throw new RuntimeException();
        base.consume(offset + strides[0] * (it >>> 1), 1, out);
        it = nextPos(it);
      }
  }
}

