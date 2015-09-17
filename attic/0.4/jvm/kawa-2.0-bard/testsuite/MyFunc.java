import gnu.mapping.*;
import gnu.math.*;
import gnu.expr.SourceName;
public class MyFunc extends Procedure2
{
  private Object arg0;

  @SourceName(name="my-func-1")
  public static final MyFunc myfunc = new MyFunc("my-func-1", IntNum.make(1));

  public MyFunc(String name, Object arg0)
  {
    super(name);
    this.arg0 = arg0;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return gnu.lists.LList.list3(arg0, arg1, arg2);
  }
}
