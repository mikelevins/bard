// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swing;
import gnu.jemacs.buffer.*;
import gnu.lists.LList;

import java.awt.Color;
import javax.swing.text.*;

public class SwingToolkit extends EToolkit
{
  public EFrame newFrame(Buffer buffer)
  {
    return new SwingFrame(buffer);
  }

  public Buffer newBuffer (String name)
  {
    return new SwingBuffer(name);
  }

  public Object getFace(String name, boolean create)
  {
    Style style = SwingBuffer.styles.getStyle(name);
    if (style == null && create)
      style = SwingBuffer.styles.addStyle(name, null);
    return style;
  }

  public void setUnderline(Object face, boolean underline)
  {
    StyleConstants.setUnderline((Style) face, underline);
  }

  public void setBold(Object face, boolean bold)
  {
    StyleConstants.setBold((Style) face, bold);
  }


  public void setForeground (Object face, Color foreground)
  {
    StyleConstants.setForeground((Style) face, foreground);
  }

  public void setBackground (Object face, Color background)
  {
    StyleConstants.setBackground((Style) face, background);
  }

  /**
   * @see gnu.jemacs.buffer.EToolkit#getMenu(gnu.lists.LList)
   */
  public EMenu getMenu(LList menubar)
  {
    return new SwingMenu(menubar);
  }

  /**
   * @see gnu.jemacs.buffer.EToolkit#getIgnoreAction()
   */
  public Object getIgnoreAction()
  {
    return IgnoreAction.getInstance();
  }
  
}
