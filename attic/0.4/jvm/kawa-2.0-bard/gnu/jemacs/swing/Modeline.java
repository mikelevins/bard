package gnu.jemacs.swing;

import gnu.jemacs.buffer.EWindow;

import javax.swing.text.StyledDocument;
import java.awt.*;

// Any point to this now?
public class Modeline extends javax.swing.JTextPane
{
  EWindow window;

  public Modeline(EWindow window, StyledDocument modelineDocument)
  {
    super(modelineDocument);
    this.window = window;
    this.setBackground(Color.lightGray);
  }

  public Dimension getMinimumSize() { return getPreferredSize(); }
  public Dimension getMaximumSize() { return getPreferredSize(); }
}
