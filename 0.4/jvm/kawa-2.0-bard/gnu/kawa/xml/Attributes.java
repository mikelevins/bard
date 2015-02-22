// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

public class Attributes extends MethodProc
{
  public static final Attributes attributes = new Attributes();
  
  public int numArgs() { return 0x1001; }

  public static void attributes (TreeList tlist, int index, Consumer consumer)
  {
    int attr = tlist.gotoAttributesStart(index);
    System.out.print("Attributes called, at:"+attr+" "); tlist.dump();
    while (attr >= 0)
      {
	int ipos = attr << 1;
	int kind = tlist.getNextKind(ipos);
	if (kind != Sequence.ATTRIBUTE_VALUE)
	  break;
	// if kind is CHAR_VALUE return text node.  FIXME
	int next = tlist.nextDataIndex(attr);
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(tlist,  ipos);
	else
	  tlist.consumeIRange(attr, next, consumer);
	attr = next;
      }
  }

  public static void attributes (Object node, Consumer consumer)
  {
    if (node instanceof TreeList)
      {
	attributes((TreeList) node, 0, consumer);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  attributes((TreeList) pos.sequence, pos.ipos >> 1, consumer);
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
	      attributes(tlist.getPosNext(index << 1), consumer);
	    else
	      attributes(tlist, index, consumer);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      attributes(node, consumer);
  }
}
