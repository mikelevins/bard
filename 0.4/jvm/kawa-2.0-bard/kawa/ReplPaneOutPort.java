package kawa;
import java.awt.Component;
import javax.swing.*;
import javax.swing.text.*;
import gnu.mapping.*;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.models.Paintable;
import gnu.kawa.models.Viewable;
import gnu.kawa.swingviews.SwingDisplay;

/** A Writer that appends its output to a ReplPane.
  * Based on code from Albert L. Ting" <alt@artisan.com>.
  */

public class ReplPaneOutPort extends OutPort
{
  ReplDocument document;
  AttributeSet style;
  String str="";
  TextPaneWriter tout;

  public ReplPaneOutPort (ReplDocument document, String path, AttributeSet style)
  {
    this(new TextPaneWriter(document, style), document, path, style);
  }

  ReplPaneOutPort (TextPaneWriter tout, ReplDocument document, String path, AttributeSet style)
  {
    super(tout, true, true, Path.valueOf(path));
    this.tout = tout;
    this.document = document;
    this.style = style;
  }

  public void write (String str, MutableAttributeSet style)
  {
    flush();
    document.write(str, style);
    setColumnNumber(1); // So freshline will Do The Right Thing.
  }

  public synchronized void write (Component c)
  {
    MutableAttributeSet style = new SimpleAttributeSet();
    StyleConstants.setComponent(style, c);
    write(" ", style);
  }

  public void print(Object v)
  {
    if (v instanceof Component)
      {
        write((Component) v);
      }
    else if (v instanceof Paintable)
      {
        MutableAttributeSet style = new SimpleAttributeSet();
        style.addAttribute(AbstractDocument.ElementNameAttribute, ReplPane.PaintableElementName);
        style.addAttribute(ReplPane.PaintableAttribute, v);
        write(" ", style);
      }
    else if (v instanceof Viewable)
      {
        MutableAttributeSet style = new SimpleAttributeSet();
        style.addAttribute(AbstractDocument.ElementNameAttribute, ReplPane.ViewableElementName);
        style.addAttribute(ReplPane.ViewableAttribute, v);
        write(" ", style);
      }
    else
      super.print(v);
  }
}

class TextPaneWriter extends java.io.Writer
{
  ReplDocument document;
  AttributeSet style;
  String str="";

  public TextPaneWriter (ReplDocument document, AttributeSet style)
  {
    this.document = document;
    this.style = style;
  }

  public synchronized void write (int x)
  {
    str = str + (char) x;
    if (x == '\n')
      flush();
  }

  public void write (String str)
  {
    document.write(str, style);
  }

  public synchronized void write (char[] data, int off, int len)
  {
    flush();
    if (len != 0)
      write(new String(data, off, len));
  }

  public synchronized void flush()
  {
    String s = str;
    if (! s.equals(""))
      {
        str = "";
	write(s);
      }
  }

  public void close ()
  {
    flush();
  }
}
