// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.util.*;

public class ConstVector<E> extends FVector<E>
{
  public ConstVector ()
  {
  }

  public ConstVector (Object[] data)
  {
    super(data);
  }

  public ConstVector(java.util.List seq)
  {
    super(new Object[seq.size()]);
    int index = 0;
    for (Iterator<?> it = seq.iterator();  it.hasNext(); )
      {
        data[index++] = it.next();
      }
  }

  public static ConstVector make(Object... data)
  {
    return new ConstVector(data);
  }

  protected void checkCanWrite ()
  {
    throw new UnsupportedOperationException();
  }

  public void setDataBackDoor(Object[] data)
  {
    this.data = data;
    this.size = data.length;
  }
}
