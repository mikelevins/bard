// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

public class Children extends MethodProc
{
  public static final Children children = new Children();
  
  public int numArgs() { return 0x1001; }

  public static void children (TreeList tlist, int index, Consumer consumer)
  {
    int child = tlist.gotoChildrenStart(index);
    if (child < 0)
      return;
    int limit = tlist.nextDataIndex(index);
    for (;;)
      {
	int ipos = child << 1;
	// If the current child is a char or primitive, skip to next real node.
	int next = tlist.nextNodeIndex(child, limit);
	// The child node wasn't primtive, so call nextDataIndex instead.
	int next0=next;
	if (next == child)
	  next = tlist.nextDataIndex(child);
	if (next < 0)
	  break;
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(tlist,  ipos);
	else
	  tlist.consumeIRange(child, next, consumer);
	child = next;
      }
  }

  public static void children (Object node, Consumer consumer)
  {
    if (node instanceof TreeList)
      {
	children((TreeList) node, 0, consumer);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  children((TreeList) pos.sequence, pos.ipos >> 1, consumer);
      }
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object node = ctx.getNextArg();
    ctx.lastArg();
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index << 1);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      children(tlist.getPosNext(index << 1), consumer);
	    else
	      children(tlist, index, consumer);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      children(node, consumer);
  }
}
