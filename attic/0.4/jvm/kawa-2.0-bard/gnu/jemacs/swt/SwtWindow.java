//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import gnu.jemacs.buffer.Buffer;
import gnu.jemacs.buffer.EKeymap;
import gnu.jemacs.buffer.EWindow;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.widgets.Composite;

/**
 * @author Christian Surlykke
 *         11-07-2004
 */
public class SwtWindow extends EWindow implements VerifyKeyListener, FocusListener, KeyListener, MouseListener
{
  private SwtWindowWidget swtWindowWidget;
  private SwtBuffer swtBuffer; 
  
  public SwtWindow(Buffer buffer) {
    this(buffer, true);
  }

  public SwtWindow(Buffer buffer, boolean wantModeLine) {
    super(buffer);
    this.swtBuffer = (SwtBuffer) buffer;
    updateModeline();
  }
  
  /**
   * 
   */
  public void getReadyToShow(final Composite parent, final int firstVisibleLine)
  {
    SwtHelper.getDisplay().syncExec(
      new Runnable() { 
        public void run() {
          swtWindowWidget = new SwtWindowWidget(parent, swtBuffer.getBufferContent(), firstVisibleLine);
          swtWindowWidget.getStyledText().addVerifyKeyListener(SwtWindow.this);
          swtWindowWidget.getStyledText().addKeyListener(SwtWindow.this);
          swtWindowWidget.getStyledText().addFocusListener(SwtWindow.this);
          swtWindowWidget.getStyledText().addMouseListener(SwtWindow.this);
          swtWindowWidget.getStyledText().forceFocus();
          swtWindowWidget.getStyledText().setCaretOffset(swtBuffer.getDot());
        }
      }
    );
    updateModeline();
  }

  
  /**
   * @see gnu.jemacs.buffer.EWindow#setBuffer(gnu.jemacs.buffer.Buffer)
   */
  public void setBuffer(Buffer buffer)
  {
    super.setBuffer(buffer);
    this.swtBuffer = (SwtBuffer) buffer;
    if (swtWindowWidget != null) {
      SwtHelper.setContent(swtWindowWidget.getStyledText(), swtBuffer.getBufferContent());
      updateModeline();
    }
  }

  
  /**
   * The purpose of this method is to emulate the 'toInt' method of SwingWindow
   * so as to transform Swt KeyEvents into the same int's as equivalent awt KeyEvents.
   * 
   * TODO: Elaborate this method so that all KeyEvents work (e.g. enter (!))
   * 
   * I've been thinkin it perhaps would be better to make EKeymap abstract with implementors 
   * for each toolkit, and then lookup commands by Swt events directly when running
   * Swt and Swing events when running swing. Must be considered more... 
   */
  private int transFormKeyKode(int swtKeyCode, int stateMask, int additionalFlags)
  {
    int characterPart = Character.toUpperCase((char) (swtKeyCode & 0xFFFF));
    int modifierPart = (stateMask & swtModifiers) >> 1; // awt modifiers seem to be displaced
                                                        // one bit to the left relative to 
                                                        // swt modifiers.
    return characterPart | modifierPart | (additionalFlags << 16);
  }

  private final static int swtModifiers = SWT.SHIFT | SWT.CTRL | SWT.ALT;

  public void handleKey (int code)
  {
    Object command = lookupKey(code);
    if (command == null )
    {
      return;
    }
    pushPrefix(code);
    pendingLength--;
    handleCommand (command);
  }


  
  public void handleCommand(Object command)
  {
    int oldDot = getBuffer().getDot();
    super.handleCommand(command);
    updateModeline();
    if (oldDot != getBuffer().getDot())
    {
      swtWindowWidget.getStyledText().showSelection();
    }
  }
  
  /**
   * @see gnu.jemacs.buffer.EWindow#setSelected()
   */
  public void setSelected()
  {
    super.setSelected();
    buffer.pointMarker.sequence = null;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#unselect()
   */
  public void unselect()
  {
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getPoint()
   */
  public int getPoint()
  {
    return SwtHelper.getCaretOffset(swtWindowWidget.getStyledText());
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#setDot(int)
   */
  public void setDot(int offset)
  {
    SwtHelper.setCaretOffset(swtWindowWidget.getStyledText(), offset);
  }
  
  public EWindow split(Buffer buffer, int lines, boolean horizontal)
  {
    SwtWindow newWindow = new SwtWindow(buffer);
    newWindow.frame = this.frame;
    linkSibling(newWindow, horizontal);
    
    int firstVisibleLine = buffer == this.buffer ? 
        SwtHelper.getTopIndex(swtWindowWidget.getStyledText()) : 0;
    int visibleLines = SwtHelper.getArea(swtWindowWidget.getStyledText()).height / 
                       SwtHelper.getLineHeight(swtWindowWidget.getStyledText());
    
    int[] weights = null;
    if (!horizontal && lines > 0 && visibleLines > 1)
    {
      weights = new int[2];
      lines = Math.min(lines, visibleLines - 1);
      weights[0] = lines;
      weights[1] = visibleLines - lines;
    }
    
    SwtHelper.injectSashFormAsParent(swtWindowWidget, horizontal ? SWT.HORIZONTAL : SWT.VERTICAL);
    newWindow.getReadyToShow(SwtHelper.getParent(swtWindowWidget), firstVisibleLine);
    if (weights != null)
      SwtHelper.setWeights(((SashForm) SwtHelper.getParent(swtWindowWidget)), weights);
    SwtHelper.layout(SwtHelper.getParent(SwtHelper.getParent(swtWindowWidget)));
    
    
    return newWindow;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getCharSize()
   */
  protected void getCharSize()
  {
    // TODO
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getWidth()
   */
  public int getWidth()
  {
    return SwtHelper.getArea(swtWindowWidget.getStyledText()).width;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#getHeight()
   */
  public int getHeight()
  {
    return SwtHelper.getArea(swtWindowWidget.getStyledText()).height;
  }

  /**
   * @see gnu.jemacs.buffer.EWindow#tooLong(int)
   */
  public Object tooLong(int pendingLength)
  {
    // TODO Something more subtle here
    return null;
  }
  
  // ---------------------------- Listener methods ------------------------------------
  
  // --- VerifyKeyListener
  public void verifyKey(VerifyEvent event)
  {
    event.doit = false;
  }

  // --- FocusListener ---
  public void focusGained(FocusEvent e)
  {
    setSelected();
  }
  
  public void focusLost(FocusEvent e)
  {
    unselect();
  }
  
  // --- KeyListener ---
  public void keyPressed(KeyEvent e)
  {
    handleKey(SwtKeyMapper.swtKey2EKey(e));
    SwtHelper.setCaretOffset(swtWindowWidget.getStyledText(), buffer.getDot());
  }
  
  public void keyReleased(KeyEvent e)
  {
  }
  
  // --- MouseListener ---
  public void mouseDoubleClick(MouseEvent e)
  {
  }

  public void mouseDown(MouseEvent e)
  {
    if (EWindow.getSelected() == this)  // Is this nessecary - aren't we always selected when this event arrives?
    {
      buffer.setDot(SwtHelper.getCaretOffset(swtWindowWidget.getStyledText()));
      SwtHelper.showSelection(swtWindowWidget.getStyledText());
    }
  }

  public void mouseUp(MouseEvent e)
  {
  }
  
  /**
   * @param e
   */
  public static void show(KeyEvent e)
  {
    System.out.println("keyCode:   " + EKeymap.show(e.keyCode));
    System.out.println("character: " + EKeymap.show(e.character));
    System.out.println("stateMask: " + EKeymap.show(e.stateMask));
  }

  public void updateModeline()
  {
    if (swtWindowWidget != null) 
    {
      SwtHelper.getDisplay().asyncExec(
        new Runnable() {
          public void run() {
            swtWindowWidget.getModeline().setText(swtBuffer.getModelineFormat().toString());
          }
        }
      );
    }
  }

}
