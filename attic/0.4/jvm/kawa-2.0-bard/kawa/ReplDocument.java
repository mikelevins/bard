package kawa;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.util.ArrayList;
import gnu.kawa.io.Path;
import gnu.kawa.io.QueueReader;
import gnu.kawa.swingviews.SwingContent;
import gnu.expr.Language;
import gnu.expr.ModuleBody;
import gnu.lists.CharBuffer;
import gnu.mapping.*;

/** A Swing document that implements a read-eval-print-loop. */

public class ReplDocument extends DefaultStyledDocument
  implements DocumentListener, FocusListener
{
  //public static javax.swing.text.html.StyleSheet styles
  // = new javax.swing.text.html.StyleSheet();
  public static StyleContext styles = new javax.swing.text.StyleContext();
  static public Style defaultStyle = styles.addStyle("default",null);
  public static Style inputStyle = styles.addStyle("input", null);
  public static Style redStyle = styles.addStyle("red", null);
  static Style blueStyle = styles.addStyle("blue", null);
  static Style promptStyle = styles.addStyle("prompt", null);
  static {
    StyleConstants.setForeground(redStyle, Color.red);
    StyleConstants.setForeground(blueStyle, Color.blue);
    StyleConstants.setForeground(promptStyle, Color.green);
    StyleConstants.setBold(inputStyle, true);
  }

  /** The pane, if any, that contains this document and has focus. */
  JTextPane pane;
  int paneCount;

  SwingContent content;

  final QueueReader in_r;
  final GuiInPort in_p;
  final ReplPaneOutPort out_stream, err_stream;

  Language language;
  Environment environment;
  Future thread;

  /** The offset where output from process is inserted. */
  public int outputMark = 0;

  /** End of pending input.
   * If {@code endMark > 0} then the area between outputMark and endMark
   * is pending input that hasn't been sent to the process yet. */
  public int endMark = -1;

  int length = 0;

  public ReplDocument (Language language, Environment penvironment,
                       boolean shared)
  {
    this(new SwingContent(), language, penvironment, shared);
  }

  private ReplDocument (SwingContent content, Language language,
                        Environment penvironment, final boolean shared)
  {
    super(content, styles);
    this.content = content;
    ModuleBody.exitIncrement();

    addDocumentListener(this);

    this.language = language;

    in_r = new QueueReader() {
        @Override
        public void checkAvailable()
        {
          checkingPendingInput();
        };
    };
    out_stream = new ReplPaneOutPort(this, "/dev/stdout", defaultStyle);
    err_stream = new ReplPaneOutPort(this, "/dev/stderr", redStyle);
    in_p = new GuiInPort(in_r, Path.valueOf("/dev/stdin"),
                         out_stream, this);

    thread = Future.make(new kawa.repl(language) {
        @Override
        public Object apply0 ()
        {
          Environment env = Environment.getCurrent();
          if (shared)
            env.setIndirectDefines();
          environment = env;
          Shell.run(language, env);
          SwingUtilities.invokeLater(new Runnable() {
              public void run() { ReplDocument.this.fireDocumentClosed(); }
            });
          return Values.empty;
        }
      }, penvironment, in_p, out_stream, err_stream);
    thread.start();
  }

    void enter ()
  {
    // In the future we might handle curses-like applicatons, which
    // outputMark may be moved backwards.  In that case the normal
    // case is that caret position >= outputMark and all the intervening
    // text has style inputStyle, in which case we do:
    // Find the first character following outputMark that is either
    // - the final newline in the buffer (in which case insert a newline
    //   at the end and treat as the following case);
    // - a non-final newline (in which case pass the intervening text
    //   and the following newline are stuffed into the in queue,
    //   and the outputMark is set after the newline);
    // - a character whose style is not inputStyle is seen (in which
    //   case the intervening text plus a final newline are stuffed
    //   into the 'in' queue, and the outputMark is moved to just
    //   before the non-inputStyle character).
    // In the second case, if there is more input following the newline,
    // we defer the rest until the inferior requests a new line.  This
    // is so any output and prompt can get properly interleaved.
    // For now, since we don't support backwards movement, we don't
    // check for inputStyle, in this case.

    // Otherwise, we do similar to Emacs shell mode:
    // Select the line containing the caret, stripping of any
    // characters that have prompt style, and add a newline.
    // That should be sent to the inferior, and copied it before outputMark.

    int pos = pane.getCaretPosition();
    CharBuffer b = content.buffer;
    int len = b.length() - 1; // Ignore final newline.
    endMark = -1;
    if (pos >= outputMark)
      {
        int lineAfterCaret = b.indexOf('\n', pos);
        if (lineAfterCaret == len)
          {
            if (len > outputMark && b.charAt(len-1) == '\n')
              lineAfterCaret--;
            else
              insertString(len, "\n", null);
          }
        endMark = lineAfterCaret;
        // Note we don't actually send the input line to the inferior
        // directly.  That happens in ReplDocument.checkingPendingInput,
        // which is invoked by the inferior thread when it is woken up here.
        // We do it this way to handle interleaving prompts and other output
        // with multi-line input, including type-ahead.
        synchronized (in_r)
          {
            in_r.notifyAll();
          }
        if (pos <= lineAfterCaret)
          pane.setCaretPosition(lineAfterCaret+1);
      }
    else
      {
        int lineBefore = pos == 0 ? 0 : 1 + b.lastIndexOf('\n', pos-1);
        Element el = getCharacterElement(lineBefore);
        int lineAfter = b.indexOf('\n', pos);
        // Strip initial prompt:
        if (el.getAttributes().isEqual(ReplDocument.promptStyle))
          lineBefore = el.getEndOffset();
        String str;
        if (lineAfter < 0)
          str = b.substring(lineBefore, len)+'\n';
        else
          str = b.substring(lineBefore, lineAfter+1);
        pane.setCaretPosition(outputMark);
        write(str, ReplDocument.inputStyle);

	if (in_r != null) {
	  in_r.append(str, 0, str.length());
	}
      }
  }

  /** Delete old text, prior to line containing outputMark. */

  public synchronized void deleteOldText ()
  {
    try
      {
        String str = getText(0, outputMark);
        int lineBefore = (outputMark <= 0 ? 0
                          : (str.lastIndexOf('\n', outputMark-1)) + 1);
        remove(0, lineBefore);
        // Changelistener updates outputMark and endMark.
      }
    catch (BadLocationException ex)
      {
        /* #ifdef use:java.lang.Throwable.getCause */
        throw new Error(ex);
        /* #else */
        // throw new WrappedException(ex);
        /* #endif */
      }
  }

  @Override
  public void insertString(int pos, String str, AttributeSet style)
  {
    try
      {
        super.insertString(pos, str, style);
      }
    catch (BadLocationException ex)
      {
        /* #ifdef use:java.lang.Throwable.getCause */
        throw new Error(ex);
        /* #else */
        // throw new WrappedException(ex);
        /* #endif */
      }
  }

  /** Insert output from the client at the outputMark.
   * Done indirectly, using SwingUtilities.invokeLater, so it can and should
   * should be called by user threads, not the event thread.
   */
  public void write (final String str, final AttributeSet style)
  {
    SwingUtilities.invokeLater(new Runnable() {
      public void run()
      {
        boolean moveCaret
          = pane != null && pane.getCaretPosition() == outputMark;
        insertString(outputMark, str, style);
        int len = str.length();
        outputMark += len;
        if (moveCaret)
         pane.setCaretPosition(outputMark);
      }
    });
  }
  
  /** Check if there is any pending input.
   * Can and should be called by user threads, not the event thread.
   * The tricky aspect is supporting type-ahead and other multi-line input,
   * since we want prompts and other output to be properly interleaved.
   */
  public void checkingPendingInput ()
  {
    SwingUtilities.invokeLater(new Runnable() {
      public void run()
      {
        // For now, we don't support moving the outputMark using escape
        // sequences, so inputStart is always just outputMark.
        int inputStart = outputMark;
        if (inputStart <= endMark)
          {
	    gnu.lists.CharBuffer b = content.buffer;
            int lineAfter = b.indexOf('\n', inputStart);
            if (lineAfter == endMark)
              endMark = -1;
            if (inputStart == outputMark) // Currently, always true.
               outputMark = lineAfter+1;
            if (in_r != null) {
              synchronized (in_r) {
                in_r.append(b, inputStart, lineAfter+1);
                in_r.notifyAll();
              }
            }
          }
      }
    });
  }

  public void focusGained(FocusEvent e)
  {
    Object source = e.getSource();
    if (source instanceof ReplPane)
      {
        pane = (ReplPane) source;
        //pane.getCaret().setDot(outputMark);
      }
    else
      pane = null;
  }

  public void focusLost(FocusEvent e)
  {
    pane = null;
  }

  public void changedUpdate (DocumentEvent e) { textValueChanged(e); }
  public void insertUpdate (DocumentEvent e) { textValueChanged(e); }
  public void removeUpdate (DocumentEvent e) { textValueChanged(e); }

  public synchronized void textValueChanged (DocumentEvent e) {
    // Update outputMark and endMark
    // Note that inserting at outputMark doesn't change outputMark,
    // That is done by the write method.
    int pos = e.getOffset();
    int delta = getLength() - length;
    length += delta;
    if (pos < outputMark)
      outputMark += delta;
    else if (pos - delta < outputMark)
      outputMark = pos;
    if (endMark >= 0)
      {
	if (pos < endMark)
	  endMark += delta;
	else if (pos - delta < endMark)
	  endMark = pos;
      }
  }

  void close () {
    in_r.appendEOF();
    // Give thread chance to finish and clean up
    try {
      Thread.sleep(100);
    } catch (InterruptedException ex) {
    }
    // Thread.stop is deprecated in JDK 1.2, but I see no good
    // alternative.  (Thread.destroy is not implemented!)
    thread.stop();
    fireDocumentClosed();
    ModuleBody.exitDecrement();
 }

  // Either null, a single DocumentCloseListener or an ArrayList of the latter.
  Object closeListeners;

  /** Register a DocumentCloseListener. */
  public void addDocumentCloseListener (DocumentCloseListener listener)
  {
    if (closeListeners == null)
      closeListeners = listener;
    else
      {
        ArrayList vec;
        if (closeListeners instanceof ArrayList)
          vec = (ArrayList) closeListeners;
        else
          {
            vec = new ArrayList(10);
            vec.add(closeListeners);
            closeListeners = vec;
          }
        vec.add(listener);
      }
  }

  public void removeDocumentCloseListener (DocumentCloseListener listener)
  {
    if (closeListeners instanceof DocumentCloseListener)
      {
        if (closeListeners == listener)
          closeListeners = null;
      }
    else if (closeListeners != null)
      {
        ArrayList vec = (ArrayList) closeListeners;
        for (int i = vec.size();  --i >= 0; )
          {
            if (vec.get(i) == listener)
              vec.remove(i);
          }
        if (vec.isEmpty())
          closeListeners = null;
      }
  }

  void fireDocumentClosed ()
  {
    if (closeListeners instanceof DocumentCloseListener)
      ((DocumentCloseListener) closeListeners).closed(this);
    else if (closeListeners != null)
      {
        ArrayList vec = (ArrayList) closeListeners;
        for (int i = vec.size();  --i >= 0; )
          ((DocumentCloseListener) vec.get(i)).closed(this);
      }
  }

  /** Listener interface for when a document closes. */
  public static interface DocumentCloseListener
  {
    /** Called when a ReplDocument closes. */
    public void closed (ReplDocument doc);
  }
}
