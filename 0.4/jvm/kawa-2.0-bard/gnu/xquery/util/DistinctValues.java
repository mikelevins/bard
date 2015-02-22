package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.math.*;

public class DistinctValues
{
  public static void distinctValues$X (Object values, NamedCollator coll,
                                       CallContext ctx)
  {
    DistinctValuesConsumer out
      = new DistinctValuesConsumer(coll, ctx.consumer);
    Values.writeValues(values, out);
  }

  /*
  public void apply (CallContext ctx) throws Throwable
  {
    Object arg = ctx.getNextArg();
    Object collation = ctx.getNextArg(null);
    ctx.lastArg();
    DistinctValuesConsumer out
      = new DistinctValuesConsumer(ctx.consumer);
    if (collation != null)
      {
	// FIXME
      }
    Values.writeValues(arg, out);
  }
  */
}

/*
class FilterValueConsumer implements Consumer
{
  Convert convert;

  public Consumer append (char v)
  {
    writeObject(convert.charToObject(v));
  }
  
  public void writeBoolean (boolean v)
  {
    writeObject(convert.booleanToObject(v));
  }

  public void writeFloat (float v)
  {
    writeObject(convert.floatToObject(v));
  }

  public void writeDouble (double v)
  {
    writeObject(convert.doubleToObject(v));
  }

  public void writeInt (int v)
  {
    writeObject(convert.intToObject(v));
  }

  public void writeLong (long v)
  {
    writeObject(convert.longToObject(v));
  }
}
*/

class DistinctValuesHashTable extends GeneralHashTable
{
  NamedCollator collator;

  public DistinctValuesHashTable (NamedCollator collator)
  {
    this.collator = collator;
  }

  public int hash (Object key)
  {
    if (key == null)
      return 0;
    if (key instanceof Number
        && (key instanceof RealNum || !(key instanceof Numeric)))
      {
        int hash = Float.floatToIntBits(((Number) key).floatValue());
        if (hash == 0x80000000) // Map -0.0 to 0
          hash = 0;
        return hash;
      }
    // This mostly-works, but isn't reliable FIXME.
    // One problem is that we ignore collation.
    return key.hashCode();
  }

  public boolean matches (Object value1, Object value2)
  {
    if (value1 == value2)
      return true;
    if (NumberValue.isNaN(value1) && NumberValue.isNaN(value2))
      return true;
    return Compare.apply(Compare.LENIENT_EQ, value1, value2, collator);
  }
}

class DistinctValuesConsumer extends FilterConsumer implements PositionConsumer
{
  DistinctValuesHashTable table;

  public DistinctValuesConsumer (NamedCollator collator, Consumer out)
  {
    super(out);
    table = new DistinctValuesHashTable(collator);
  }

  public void writePosition(SeqPosition position)
  {
    writeObject(position);
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    writeObject(((NodeTree) seq).typedValue(ipos));
  }

  public void writeBoolean(boolean v)
  {
    writeObject(v ? Boolean.TRUE : Boolean.FALSE);
  }

  public void writeObject (Object value)
  {
    if (value instanceof Values)
      {
	Values.writeValues(value, this);
	return;
      }
    if (value instanceof KNode)
      {
        KNode node = (KNode) value;
        writeObject(((NodeTree) node.sequence).typedValue(node.ipos));
        return;
      }
    Object old = table.get(value, null);
    if (old != null)
      return;
    table.put(value, value);
    base.writeObject(value);
  }
}
