// Copyright (c) 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.Procedure;
import gnu.mapping.Values;
import gnu.lists.*;
import java.io.*;
import gnu.expr.*;

/** A type that matches some number of repetitions of a basetype. */

public class OccurrenceType extends ObjectType
  implements Externalizable, TypeValue
{
  Type base;
  int minOccurs;
  int maxOccurs;

  public Type getBase () { return base; }
  public int minOccurs() { return minOccurs; }
  public int maxOccurs() { return maxOccurs; }

  public OccurrenceType (Type base, int minOccurs, int maxOccurs)
  {
    this.base = base;
    this.minOccurs = minOccurs;
    this.maxOccurs = maxOccurs;
  }

  public static Type getInstance (Type base, int minOccurs, int maxOccurs)
  {
    if (minOccurs == 1 && maxOccurs == 1)
      return base;
    if (minOccurs == 0 && maxOccurs < 0
        && (base == SingletonType.instance || base == Type.pointer_type))
      return Type.pointer_type;
    if (base instanceof OccurrenceType) {
        OccurrenceType occ = (OccurrenceType) base;
        minOccurs *= occ.minOccurs;
        maxOccurs = maxOccurs < 0 || occ.maxOccurs < 0 ? -1
            : maxOccurs * occ.maxOccurs;
        base = occ.base;
    }
    return new OccurrenceType(base, minOccurs, maxOccurs);
  }

  public static final Type emptySequenceType =
    OccurrenceType.getInstance(SingletonType.instance, 0, 0);

  public Type getImplementationType()
  {
    return Type.pointer_type;
  }

  public int compare(Type other)
  {
    if (other instanceof LazyType)
      other = ((LazyType) other).getValueType();
    if (other instanceof OccurrenceType)
      {
        OccurrenceType occOther = (OccurrenceType) other;
        if (minOccurs == occOther.minOccurs
            && maxOccurs == occOther.maxOccurs)
          return base.compare(occOther.getBase());
      }
    /*
    Type primeThis = itemPrimeType(getBase());
    Type primeOther = itemPrimeType(other);
    FIXME: Compare primThis with primOther AND the occurrence numbers.
    */
    return -2;
  }

  public Object coerceFromObject (Object obj)
  {
    if (obj instanceof Values)
      {
      }
    else
      {
        // Assumes that base is an item type.  FIXME.
	if (minOccurs <= 1 && maxOccurs != 0)
          return base.coerceFromObject(obj);
      }
    // FIXME
    if (! isInstance(obj))
      throw new ClassCastException();
    return obj;
  }

  public boolean isInstance (Object obj)
  {
    if (obj instanceof Values)
      {
	Values vals = (Values) obj;
	int pos = vals.startPos();
	int n = 0;
	if (base instanceof ItemPredicate)
	  {
	    ItemPredicate pred = (ItemPredicate) base;
	    for (;;)
	      {
		boolean matches;
		matches = pred.isInstancePos(vals, pos);
		pos = vals.nextPos(pos);
		if (pos == 0)
		  {
		    return n >= minOccurs
		      && (maxOccurs < 0 || n <= maxOccurs);
		  }
		if (! matches)
		  return false;
		n++;
	      }
	  }
	else
	  {

	    for (;;)
	      {
		pos = vals.nextPos(pos);
		if (pos == 0)
		  {
		    return n >= minOccurs
		      && (maxOccurs < 0 || n <= maxOccurs);
		  }
		Object value = vals.getPosPrevious(pos);
		if (! base.isInstance(value))
		  return false;
		n++;
	      }
	  }
      }
    else
      {
	if (minOccurs > 1 || maxOccurs == 0)
	  return false;
	return base.isInstance(obj);
      }
  }

  public void emitTestIf(Variable incoming, Declaration decl,
			 Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    if (decl != null)
      {
	code.emitDup();
	decl.compileStore(comp);
      }
    comp.compileConstant(this);
    code.emitSwap();
    code.emitInvokeVirtual(isInstanceMethod);
    code.emitIfIntNotZero();
  }

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
  }

  public Expression convertValue (Expression value)
  {
    return null;
  }

  public Procedure getConstructor ()
  {
    return null;
  }

  /** Return a conservative estimate on the min/max number of items of a type.
   * @return {@code maxCount << 12 | minCount & 0xFFF},
   * where a {@code maxCount} of -1 means unbounded.
   */
  public static int itemCountRange (Type type)
  {
    if (type instanceof SingletonType)
      return (1 << 12) | 1;
    if (type instanceof OccurrenceType)
      {
        OccurrenceType occ = (OccurrenceType) type;
        int min = occ.minOccurs();
        int max = occ.maxOccurs();
        int bnum = itemCountRange(occ.getBase());
        if ((min == 1 && max == 1)
            || bnum == 0)
          return bnum;
        if (max > 0xfffff)
          max = -1;
        if (max == 0)
          return 0;
        int bmin = bnum & 0xfff;
        int bmax = bnum >> 12;
        if (bnum != 0x1001)
          {
            if (min > 0xfff)
              min = 0xfff;
            min = min * bmin;
            if (min > 0xfff)
              min = 0xfff;
            if (max < 0 || bmax < 0)
              max = -1;
            else
              max = max * bmax;
            if (max > 0xfffff)
              max = -1;
          }
        return (max << 12) | min;
      }
    if (type instanceof PrimType)
      return type.isVoid() ? 0 : 0x1001;
    if (type instanceof ArrayType)
      return 0x1001;
    if (type instanceof ObjectType)
      {
	int cmp = type.compare(Compilation.typeValues);
	if (cmp == -3)
	  return 0x1001;
      }
    return -1 << 12;
  }

  /** Returna a quantifer kind for a sequence type.
   * @return '0' if type is known to be a void (0-item) type;
   *         '1' if type is known to be a single-item type;
   *         '?' if type matches a sequence of 0 or 1 items;
   *         '+' if type matches a sequence of 1 or more items;
   *         '*' otherwise.
   */
  public static char itemCountCode (Type type)
  {
    int num = itemCountRange(type);
    int min = num & 0xFFF;
    int max = num >> 12;
    return max == 0 ? '0'
      : min == 0 ? (max == 1 ? '?' : '*')
      : min == 1 && max == 1 ? '1' : '+';
  }

  public static boolean itemCountIsZeroOrOne (Type type)
  {
    // cute hack for: max == 0 || max == 1.
    return (itemCountRange(type) >> 13) == 0;
  }

  public static boolean itemCountIsOne (Type type)
  {
    return itemCountRange(type) == 0x1001;
  }

  /** QUery formal semantics "prime type"
   */
  public static Type itemPrimeType (Type type)
  {
    while (type instanceof OccurrenceType)
      type = ((OccurrenceType) type).getBase();
    return itemCountIsOne(type) ? type : SingletonType.instance;
      
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(base);
    out.writeInt(minOccurs);
    out.writeInt(maxOccurs);
  }

  public String toString ()
  {
    String b = base.toString();
    boolean parens = b == null || b.indexOf(' ') >= 0;
    StringBuffer sbuf = new StringBuffer();
    if (parens)
      sbuf.append('(');
    sbuf.append(b);
    if (parens)
      sbuf.append(')');
    if (minOccurs == 1 && maxOccurs == 1)
      ;
    else if (minOccurs == 0 && maxOccurs == 1)
      sbuf.append('?');
    else if (minOccurs == 1 && maxOccurs == -1)
      sbuf.append('+');
    else if (minOccurs == 0 && maxOccurs == -1)
      sbuf.append('*');
    else
      {
	sbuf.append('{');
	sbuf.append(minOccurs);
	sbuf.append(',');
	if (maxOccurs >= 0)
	  sbuf.append(maxOccurs);
	else
	  sbuf.append('*');
	sbuf.append('}');
      }
    return sbuf.toString();
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    base = (Type) in.readObject();
    minOccurs = in.readInt();
    maxOccurs = in.readInt();
  }

    /* #ifndef JAVA8 */
    // public String encodeType(Language language) { return null; }
    /* #endif */

  public static final ClassType typeOccurrenceType
    = ClassType.make("gnu.kawa.reflect.OccurrenceType");
  static final Method isInstanceMethod
    = typeOccurrenceType.getDeclaredMethod("isInstance", 1);
}
