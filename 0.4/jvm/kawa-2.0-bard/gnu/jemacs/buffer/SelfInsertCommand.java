// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.buffer;
import gnu.math.IntNum;
import gnu.mapping.*;

public class SelfInsertCommand extends Procedure0or1
{
  public Object getProperty (Object key, Object defaultValue)
  {
    if (key == "emacs-interactive")
      return "*p";
    return super.getProperty(key, defaultValue);
  }

  public Object apply0 ()
  {
    apply1 (IntNum.one());
    return Values.empty;
  }


  public Object apply1 (Object arg1)
  {
    int count = ((Number) arg1).intValue();
    EWindow window = EWindow.getSelected();
    Buffer buffer = window.buffer;

    char ch = (char) window.pendingKeys[window.pendingLength];
    buffer.insertChar(ch, count);
    return Values.empty;
  }
}
