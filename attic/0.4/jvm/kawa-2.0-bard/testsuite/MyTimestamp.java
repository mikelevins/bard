public class MyTimestamp extends MyDate
{
  int t;
  public MyTimestamp(int d, int t) { super(d); this.t = t; }
  public int myCompareTo(MyTimestamp ts)
  {
    return d < ts.d ? -1 : d > ts.d ? 1
      : t < ts.t ? -1 : t > ts.t ? 1 : 0;
  }

  public int myCompareTo(Object o)
  {
    System.err.println( "in MyTimestamp(Object)" );
    return myCompareTo((MyTimestamp)o);
  }
            
}
