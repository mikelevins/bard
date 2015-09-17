package kawa.lang;
import gnu.lists.*;
import java.io.*;
import gnu.text.Printable;

public class PairPat extends Pattern implements Printable, Externalizable
{
  Pattern car;
  Pattern cdr;
  private int car_count, cdr_count;

  public PairPat ()
  {
  }

  public PairPat (Pattern car, Pattern cdr)
  {
    this.car = car;
    this.cdr = cdr;
    car_count = car.varCount ();
    cdr_count = cdr.varCount ();
  }

  public static PairPat make (Pattern car, Pattern cdr)
  {
    return new PairPat (car, cdr);
  }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    if (! (obj instanceof Pair))
      return false;
    Pair pair = (Pair) obj;
    return (car.match (pair.getCar(), vars, start_vars)
	    && cdr.match (pair.getCdr(), vars, start_vars + car_count));
  }

  public void print (Consumer out)
  {
    out.write("#<pair-pattern car: ");
    car.print(out);
    out.write(" cdr: ");
    cdr.print(out);
    out.write('>');
  }

  public int varCount () { return car_count + cdr_count; }

  /**
   * @serialData Write the car and then the cdr patterns (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(car);
    out.writeObject(cdr);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    car = (Pattern) in.readObject();
    cdr = (Pattern) in.readObject();
  }
}
