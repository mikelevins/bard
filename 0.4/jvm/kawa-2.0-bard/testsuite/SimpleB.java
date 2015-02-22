public class SimpleB extends SimpleA
{
  public int c = a + b;

  public int f (int y) { return 1000 + super.f(y); }

  private int i;
  public int getI() { return i; }
  public void setI(int i) { this.i = i; }
}
