// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swing;
import gnu.jemacs.buffer.*;

import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.util.Hashtable;

/** An Emacs window (EWindow) implemented using the Swing toolkits. */

public class SwingWindow extends EWindow
implements java.awt.event.FocusListener,
  java.awt.event.KeyListener,
  javax.swing.event.ChangeListener
{
  /* Map JTextPane to SwingWindow. */
  static Hashtable panemap = new Hashtable();

  JTextPane jtextpane;
  /** The panel that contains this window and the modeline. */
  JPanel panel;
  JScrollPane scrollPane;

  public Modeline modeline;

  public SwingWindow(Buffer buffer)
  {
    this(buffer, true);
  }

  /** Create new Window.
   * @param buffer the Buffer containing the data.
   * @param wantModeline true if we should create a mode line
   */
  public SwingWindow(Buffer buffer, boolean wantModeline)
  {
    super(null);
    jtextpane = new JTextPane(((SwingBuffer) buffer).doc);
    panemap.put(jtextpane, this);
    if (wantModeline)
      modeline = new Modeline(this, ((SwingBuffer) buffer).modelineDocument);
    this.buffer = buffer;
    jtextpane.addFocusListener(this);
    jtextpane.addKeyListener(this);
  }

  /** Warp this (and optional modeline) inside a ScrollPane in a new JPanel. */
  public JPanel wrap()
  {
    BorderLayout layout = new BorderLayout();
    JPanel panel = new JPanel(layout);
    scrollPane = new JScrollPane(jtextpane,
                                 JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    panel.add(scrollPane, BorderLayout.CENTER);
    if (modeline != null)
      panel.add(modeline, BorderLayout.SOUTH);
    this.panel = panel;
    return panel;
  }

  /** Get the JPanel containing this Window. */
  public JPanel getPanel()
  {
    return panel;
  }

  public void focusGained(FocusEvent e)
  {
    setSelected();
  }

  public void focusLost(FocusEvent e)
  {
  }

  public void requestFocus()
  {
    jtextpane.requestFocus();
  }

  public void setBuffer (Buffer buffer)
  {
    if (this.buffer == buffer)
      return;

    super.setBuffer(buffer);
    jtextpane.setDocument(((SwingBuffer) buffer).doc);
    if (modeline != null)
      modeline.setDocument(((SwingBuffer) buffer).modelineDocument);
    EWindow selected = getSelected();
    if (selected == this)
      {
	unselect();
	// Change buffer's pointMarker so it follows this EWindow's Caret.
	Caret caret = jtextpane.getCaret();
	caret.setDot(buffer.getDot());
	select(caret);
      }
  }

  public void unselect()
  {
    Caret caret = ((SwingBuffer) buffer).curPosition;
    if (caret == null)
      return;
    int point = caret.getDot();
    int index = ((SwingBuffer) buffer).content.buffer.createPos(point, true);
    buffer.pointMarker.ipos = index;
    ((SwingBuffer) buffer).curPosition = null;
    jtextpane.getCaret().removeChangeListener(this);
    // ?? selected = null;
  }

  public void setSelected()
  {
    super.setSelected();
    if (jtextpane != null)
      select(jtextpane.getCaret());
  }

  public int getPoint()
  {
    return 1 + jtextpane.getCaret().getDot();
  }

  public void setDot(int offset)
  {
    jtextpane.getCaret().setDot(offset);
  }

  public EWindow split (Buffer buffer, int lines, boolean horizontal)
  {
    SwingWindow window = new SwingWindow(buffer);
    EFrame frame = this.frame;
    window.frame = frame;
    linkSibling(window, horizontal);
    window.modeline = new Modeline(window, ((SwingBuffer) buffer).modelineDocument);

    JPanel panel = this.getPanel();
    java.awt.Dimension oldSize = panel.getSize();
    java.awt.Container oldParent = panel.getParent();
    oldParent.remove(panel);
    JSplitPane split
      = new JSplitPane(horizontal ? JSplitPane.HORIZONTAL_SPLIT
                       : JSplitPane.VERTICAL_SPLIT,
                       panel, window.wrap());
    split.setDividerSize(2);
    // FIXME - use lines.
    split.setDividerLocation((horizontal ? oldSize.width : oldSize.height) / 2);
    oldParent.add(split);
    oldParent.validate();
    if (this == EWindow.getSelected())
      this.requestFocus();
    return window;
  }

  private void select(Caret caret)
  {
    // Change buffer's pointMarker so it follows this EWindow's Caret.
    ((SwingBuffer) buffer).curPosition = caret;
    if (! buffer.pointMarker.isPoint())
      ((SwingBuffer) buffer).content.buffer.releasePos(buffer.pointMarker.ipos);
    buffer.pointMarker.sequence = null;
    caret.addChangeListener(this);
  }

  public void stateChanged(javax.swing.event.ChangeEvent e)
  {
    Object source = e.getSource();
    if (source instanceof Caret && buffer != null)
      {
	Caret caret = (Caret) source;
	int mark = caret.getMark();
	int dot = caret.getDot();
	if (mark != dot)
	  {
	    buffer.markMarker.set(buffer, mark);
	  }
      }
  }

  public static JTextPane getContainedWindow(Container cont, boolean last)
  {
    for (;;)
      {
        if (cont instanceof JTextPane)
          return (JTextPane) cont;
        if (cont instanceof JScrollPane)
          cont = (Container) ((JScrollPane) cont).getViewport().getView();
        else if (cont instanceof JFrame)
          cont = ((JFrame) cont).getContentPane();
        else if (cont instanceof JSplitPane)
          {
            JSplitPane split = (JSplitPane) cont;
            cont = (Container)
              (last ? split.getRightComponent() : split.getLeftComponent());
          }
        else
          {
            int count = cont.getComponentCount();
            if (count == 0)
              return null;
            cont = (Container) cont.getComponent(last ? (count - 1) : 0);
          }
      }
  }

  /*
  public SwingWindow getNextWindow(boolean forwards)
  {
    Container prev = this;
    for (;;)
      {
        Container parent = prev.getParent();
        if (parent == null)
          return null;
        if (parent instanceof JSplitPane)
          {
            JSplitPane split =(JSplitPane) parent;
            if (prev == split.getLeftComponent())
              {
                if (forwards)
                  return getFirstWindow((Container) split.getRightComponent());
              }
            else // prev == split.getRightComponent():
              {
                if (!forwards)
                  return getLastWindow((Container) split.getLeftComponent());
              }
          }
        prev = parent;
      }
  }
  */

  public static SwingWindow getWindow(java.awt.event.ActionEvent event)
  {
    // Maybe use TextAction.getTextComponent instead? FIXME.
    Component component = (Component) event.getSource();
    for (;;)
      {
        if (component instanceof JTextPane)
          return (SwingWindow) panemap.get(component);
        component = component.getParent();
      }
  }

  protected void deleteNoValidate()
  {
    super.deleteNoValidate();
    if (modeline != null)
      panel.remove(modeline);
    panel.remove(scrollPane);

    // Mow remove the Panel from its parent.
    Container oldParent = panel.getParent();
    if (oldParent instanceof JSplitPane)
      {
        JSplitPane split = (JSplitPane) oldParent;
        Component other;
        if (panel == split.getLeftComponent())
          other = split.getRightComponent();
        else
          other = split.getLeftComponent();
        split.remove(jtextpane);
        split.remove(other);
        // In the JSplitPane's parent, replace the JSplitPane by just other 
        Container splitParent = split.getParent();
        if (splitParent instanceof JSplitPane)
          {
            JSplitPane outerSplit = (JSplitPane) splitParent;
            if (split == outerSplit.getLeftComponent())
              outerSplit.setLeftComponent(other);
            else
              outerSplit.setRightComponent(other);
          }
        else
          {
            splitParent.remove(split);
            splitParent.add(other);
          }
      }
    else
      oldParent.remove(panel);

    panemap.remove(jtextpane);
    jtextpane = null;
    panel = null;
    scrollPane = null;
  }

  public void activateRegion ()
  {
    Caret caret = jtextpane.getCaret();
    caret.setDot(buffer.markMarker.getOffset());
    caret.moveDot(buffer.getDot());
  }

  public Dimension getPreferredScrollableViewportSize()
  {
    Dimension size = panel.getSize();
    if (modeline != null)
      size = new Dimension(size.width,
                       size.height - modeline.getPreferredSize().height);
    return size;
  }

  protected void getCharSize()
  {
    java.awt.Font defaultFont
      = ((SwingBuffer) buffer).doc.getFont(SwingBuffer.defaultStyle);
    java.awt.FontMetrics fm
      = jtextpane.getGraphics().getFontMetrics(defaultFont);
    charHeight = fm.getHeight();
    charWidth = fm.charWidth('m');
  }

  public int getWidth ()
  {
    return jtextpane.getWidth();
  }

  public int getHeight ()
  {
    return jtextpane.getHeight();
  }

  public void keyTyped(KeyEvent e)
  {
    handle(e, toInt(e, 0));
  }

  public void keyPressed(KeyEvent e)
  {
    handle(e, toInt(e, EKeymap.PRESSED));
  }

  public void keyReleased(KeyEvent e)
  {
    handle(e, toInt(e, EKeymap.RELEASED));
  }

  protected void handle(KeyEvent e, int code)
  {
    Object action = lookupKey(code);

    pushPrefix(code);
    pendingLength--;

    if (action == null || action instanceof IgnoreAction)
      {
	e.consume();
	return;
      }
    handleCommand(action);
    e.consume();
  }

  int toInt(KeyEvent e, int kind)
  {
    int mods = e.getModifiers();
    if (kind == 0)
      mods &= ~ EKeymap.SHIFT_MASK;
    return (kind == 0 ? e.getKeyChar() : e.getKeyCode())
      | ((mods | kind) << 16);
  }

/**
 * 
 */
void flushPending() 
{
  pendingLength = 0;
}

/**
 * @see gnu.jemacs.buffer.EWindow#tooLong(int)
 */
public Object tooLong(int pendingLength)
{
  return new TooLongAction(pendingLength);
}

}
