package gnu.jemacs.buffer;
import gnu.mapping.*;
import gnu.expr.*;

/** Represents a "mode instance" - a mode active for a specific {@link Buffer}. */

public class Mode extends ModuleBody
{
  public Buffer buffer;
  public Mode next;

  public Buffer getBuffer()
  {
    return buffer;
  }

  public Object apply0(ModuleMethod proc)
  {
    return applyN(proc, Values.noArgs);
  }

  public Object apply1(ModuleMethod proc, Object arg1)
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return applyN(proc, args);
  }

  public Object apply2(ModuleMethod proc, Object arg1, Object arg2)
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return applyN(proc, args);
  }

  public Object apply3(ModuleMethod proc,
                       Object arg1, Object arg2, Object arg3)
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return applyN(proc, args);
  }

  public Object apply4(ModuleMethod proc,
                       Object arg1, Object arg2, Object arg3, Object arg4)
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return applyN(proc, args);
  }

  public Object applyN(ModuleMethod proc, Object[] args)
  {
    int count = args.length;
    int num = proc.numArgs();
    if (count >= (num & 0xFFF)
	&& (num < 0 || count <= (num >> 12)))
      {
        switch (count)
          {
          case 0:
            return apply0(proc);
          case 1:
            return apply1(proc, args[0]);
          case 2:
            return apply2(proc, args[0], args[1]);
          case 3:
            return apply3(proc, args[0], args[1], args[2]);
          case 4:
            return apply4(proc, args[0], args[1], args[2], args[3]);
          }
      }
    throw new WrongArguments(proc, count);
  }
}
