// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.mapping.*;
import java.io.*;

/** Abstract class that scans part of a node tree.
 * Takes a node argument, and writes matching "relative" nodes
 * out to a PositionConsumer as a sequence of position pairs.
 * This is uses to implement "path expressions" as in XPath/XSLT/XQuery.
 * For example, the ChildAxis sub-class writes out all child nodes
 * of the argument that match the 'type' NodePredicate.
 */

public abstract class TreeScanner extends MethodProc
  implements Externalizable
{
  TreeScanner ()
  {
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.xml.CompileXmlFunctions:validateApplyTreeScanner");
  }

  public NodePredicate type;

  public NodePredicate getNodePredicate () { return type; }

  public abstract void scan (AbstractSequence seq, int ipos,
			     PositionConsumer out);

  public int numArgs() { return 0x1001; }

  public void apply (CallContext ctx)  throws Throwable
  {
    PositionConsumer out = (PositionConsumer) ctx.consumer;
    Object node = ctx.getNextArg();
    ctx.lastArg();
    KNode spos;
    try
      {
        spos = (KNode) node;
      }
    catch (ClassCastException ex)
      {
        throw new WrongType(getDesc(), WrongType.ARG_CAST, node, "node()");
      }
    scan(spos.sequence, spos.getPos(), out);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(type);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    type = (NodePredicate) in.readObject();
  }

  public String getDesc ()
  {
    String thisName = getClass().getName();
    int dot = thisName.lastIndexOf('.');
    if (dot > 0)
      thisName = thisName.substring(dot+1);
    return thisName+"::"+type;
  }

  public String toString ()
  {
    return "#<" + getClass().getName() + ' ' + type + '>';
  }
}
