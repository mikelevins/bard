public class MyDate
{
  int d;
  public MyDate(int d) { this.d = d; }
  public int myCompareTo(MyDate ts)
  {
    System.err.println( "in MyDate" );
    return d < ts.d ? -1 : d > ts.d ? 1 : 0;
  }
}
