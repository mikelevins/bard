// Copyright (c) 2008 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

public class ImmutablePair extends Pair
{
  public ImmutablePair (Object carval, Object cdrval)
  {
    super(carval, cdrval);
  }

  public ImmutablePair ()
  {
  }

  public void setCar (Object car)
  {
    throw new UnsupportedOperationException("cannot modify read-only pair");
  }

  public void setCdr (Object cdr)
  {
    throw new UnsupportedOperationException("cannot modify read-only pair");
  }
}
