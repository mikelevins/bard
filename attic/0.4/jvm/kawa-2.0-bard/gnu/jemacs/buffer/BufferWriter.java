package gnu.jemacs.buffer;
import java.awt.Color;

/** A Writer that writes at a Buffer's point or a Marker. */

public class BufferWriter extends java.io.Writer implements Runnable
{
  Marker marker;
  Object style;
  Object stylePlain;
  boolean adjustPoint;

  /** We are not handling escape sequences. */
  static final int NO_ESCAPES_STATE = 1;
  /** We are ready to handle escape sequences. */
  static final int NORMAL_STATE = 0;
  /** Last character was a graphic in the last column.
   * If next char is graphic, first move one column right
   * (and line warp) before displaying it. */
  static final int LAST_COLUMN_STATE = 1;
  /** Seen an Escape character. */
  static final int SEEN_ESC_STATE = 2;
  /** We're in the middle of an escape sequence.
   * What we've seen so far is in the savedOutput buffer. */
  static final int SEEN_ESC_LBRAC_STATE = 3;
  static final int SEEN_ESC_RBRAC_STATE = 4;
  int state = NORMAL_STATE;

  static final int ESC = 033;

  boolean insertMode = false;

  char[] savedOutput;
  int savedCount;

  public BufferWriter (Marker marker, boolean adjustPoint)
  {
    this.marker = marker;
    EToolkit toolkit = EToolkit.getInstance();
    this.stylePlain = toolkit.getFace("output", true);
    this.style = stylePlain;
    this.adjustPoint = adjustPoint;
    // StyleConstants.setItalic(this.style, true);
  }

  public BufferWriter (Buffer buffer)
  {
    this(buffer.pointMarker, false);
  }

  boolean bold = false;
  boolean underline = false;
  boolean blink = false;  // not implemented
  boolean inverse;
  boolean invisible;
  String foregroundName;
  String backgroundName;
  Color foreground;
  Color background;
  StringBuffer styleNameBuf;
  String styleName;

  void resetAttributes()
  {
    bold = false;
    underline = false;
    blink = false;
    inverse = false;
    invisible = false;
    foregroundName = null;
    backgroundName = null;
    foreground = null;
    background = null;
  }

  void updateStyle()
  {
    if (styleNameBuf == null)
      styleNameBuf = new StringBuffer(60);
    styleNameBuf.setLength(0);
    if (underline) styleNameBuf.append("underlined,");
    if (bold) styleNameBuf.append("bold,");
    if (foreground != null)
      {
	styleNameBuf.append("fg=");
	styleNameBuf.append(foregroundName != null ? foregroundName
			    : foreground.toString());
	styleNameBuf.append(',');
      }
    if (background != null)
      {
	styleNameBuf.append("bg=");
	styleNameBuf.append(backgroundName != null ? backgroundName
			    : background.toString());
	styleNameBuf.append(',');
      }
    int slen = styleNameBuf.length();
    if (slen == 0)
      {
	style = stylePlain;
	styleName = "output";
	return;
      }
    styleNameBuf.setLength(slen-1); // Remove final comma.
    styleName = styleNameBuf.toString();
    EToolkit toolkit = EToolkit.getInstance();
    style = toolkit.getFace(styleName, false);
    if (style != null)
      return;
    style = toolkit.getFace(styleName, true);
    toolkit.setUnderline(style, underline);
    toolkit.setBold(style, bold);
    if (foreground != null)
      toolkit.setForeground(style, foreground);
    if (background != null)
      toolkit.setBackground(style, background);
  }

  private String name;
  private Color color;
  private void getColor(int index, boolean bright /*ignored, for now*/)
  {
    switch (index)
      {
      case 0:  color = Color.black;   name = "black";   break;
      case 1:  color = Color.red;     name = "red";     break;
      case 2:  color = Color.green;   name = "green";   break;
      case 3:  color = Color.yellow;  name = "yellow";  break;
      case 4:  color = Color.blue;    name = "blue";    break;
      case 5:  color = Color.magenta; name = "magenta"; break;
      case 6:  color = Color.cyan;    name = "cyan";    break;
      case 7:  color = Color.white;   name = "white";   break;
      default: color = null;          name = null;
      }
  }

  /**
   * Process an SGR command with the given code.
   * @param param parameter value from the escape sequence
   * @param position following offset in savedOutput array
   * @return updated value of position, if we gobble multiple parameters
   */
  public int handleSetCharacterRendition(int param, int position)
  {
    switch (param)
      {
      case -1: case 0:
	resetAttributes();
      case 1:
	bold = true;
	break;
      case 4:
	underline = true;
	break;
      case 22:
	bold = false;
	break;
      case 24:
	underline = false;
	break;
      default:
	if (param >= 30 && param <= 39)
	  {
	    getColor(param - 30, false);
	    foreground = color;
	    foregroundName = name;
	  }
	else if (param >= 40 && param <= 49)
	  {
	    getColor(param - 40, false);
	    background = color;
	    backgroundName = name;
	  }
	else if (param >= 90 && param <= 97)
	  {
	    getColor(param - 90, true);
	    background = color;
	    backgroundName = name;
	  }
	else if (param >= 100 && param <= 107)
	  {
	    getColor(param - 100, true);
	    background = color;
	    backgroundName = name;
	  }
	// else ignore
      }
    return position;
  }

  public void handleOperatingSystemCommand(char ch)
  {
    if (ch == '\007') /* BEL */
      {
	// ignore, for now
	state = NORMAL_STATE;
      }
    else if (savedCount >= savedOutput.length)
      {
	// Seems rather excessive ...  Probably error.
	int i;
	for (i = 0;  i < savedCount && savedOutput[i] != '\n';) i++;
	if (i < savedCount || ch == '\n')
	  state = NORMAL_STATE; // Seen '\n' - OK time to bail.
	else
	  savedCount = 0;  // No '\n' yet - drop what we've seen so far.
      }
    else
      savedOutput[savedCount++] = ch;
  }

  /**
   * Process a single command following CSI.
   * CSI is "Control Sequence Introducer" - i.e. ESC [.
   * @param ch the control command
   * @param param parameter value from the escape sequence
   * @param position following offset in savedOutput array
   * @return updated value of position, if we gobble multiple parameters
   */
  public int handleCSICommand(char ch, int param, int position)
  {
    switch (ch)
      {
      case 'C':  // \E[C - cursor right
	moveColumns(param > 0 ? param : 1);
	break;
      case 'D':  // \E[D - cursor left
	moveColumns(- (param > 0 ? param : 1));
	break;
      case 'h':  // \E[?h - DEC Private Mode Set
	if (param == 4)
	  insertMode = true;
	break;
      case 'l':  // \E[?l - DEC Private Mode Reset
	if (param == 4)
	  insertMode = false;
	break;
      case 'm':
	return handleSetCharacterRendition(param, position);
      }
    return position;
  }

  public void handleEscapeBracket(char ch)
  {
    if (ch == ';' || (ch >= '0' && ch <= '9'))
      {
	if (savedCount >= savedOutput.length)
	  savedCount = 0;  // Overflow - drop excess parameters.
	savedOutput[savedCount++] = ch;
      }
    else
      {
	int value = -1;
	for (int i = 0;  i < savedCount;  i++)
	  {
	    ch = savedOutput[i];
	    if (ch >= '0' && ch <= '9')
	      value = (value <= 0 ? 0 : 10 * value) + (ch - '0');
	    else
	      {
		i = handleCSICommand('m', value, i);
		value = -1;
	      }
	  }
	handleCSICommand('m', value, savedCount);
	updateStyle();
	state = NORMAL_STATE;
      }
  }

  public void unTabifyRestOfLine()
  {
    // FIXME
  }

  /** Delete characters - but only in current screen line. */
  public void removeChars(int count)
  {
    int save = marker.getOffset();
    moveColumns(count);
    marker.removeChar(marker.getOffset() - save);
  }

  /** Move some number of columns right (or left if count < 0). */
  public void moveColumns(int count)
  {
    marker.moveToColumn(marker.currentColumn() + count, true);
  }

  /*
  public synchronized void put(int x)
  {
    if (insertMode)
      unTabifyRestOfLine();
    else if (marker.getOffset() < marker.buffer.maxDot())
      removeChars(1);
    AbstractDocument document = marker.buffer;
    boolean mustAdjustPoint
      = adjustPoint && marker.getOffset() == marker.buffer.getDot();
    marker.insertChar((char) x, 1, x == '\n' ? stylePlain : style);
    if (mustAdjustPoint)
      marker.buffer.setDot(marker.getOffset());
  }
  */

  public synchronized void put (char[] data, int off, int len)
  {
    if (len == 0)
      return;
    if (insertMode)
      unTabifyRestOfLine();
    else
      removeChars(len);
    boolean mustAdjustPoint
      = adjustPoint && marker.getOffset() == marker.buffer.getDot();
    marker.insert(new String(data, off, len), style);
    if (mustAdjustPoint)
      marker.buffer.setDot(marker.getOffset());
  }

  char[] buf1 = new char[1];

  public synchronized void write(int ch)
  {
    boolean move = marker.getOffset() == marker.buffer.getDot();
    write1(ch);
    if (move) marker.buffer.setDot(marker.getOffset());
  }

  public synchronized void write1(int ch)
  {
    if (state <= NORMAL_STATE)
      {
	if (ch >= ' ' || ch == '\n' || state < NORMAL_STATE)
	  {
	    buf1[0] = (char) ch;
	    put(buf1, 0, 1);
	  }
	else if (ch == ESC)
	  state = SEEN_ESC_STATE;
	else if (ch == '\b')
	  moveColumns(-1);
	else if (ch == '\t')
	  {
	    int col = marker.currentColumn();
	    marker.moveToColumn(col + 8 - (col & 7), true);
	  }
	else if (ch == '\r')
	  {
	    // FIXME - until we handle '\n' more generally.
	    // marker.moveToColumn(0, false);
	  }
	else
	  System.err.println("received ctrl-"+(char)(ch+64));
      }
    else if (state == SEEN_ESC_STATE)
      {
	switch (ch)
	  {
	  case '[':
	    state = SEEN_ESC_LBRAC_STATE;
	    if (savedOutput == null)
	      savedOutput = new char[100];
	    savedCount = 0;
	    break;
	  case ']':
	    state = SEEN_ESC_RBRAC_STATE;
	    if (savedOutput == null)
	      savedOutput = new char[100];
	    savedCount = 0;
	    break;
	  default:
	    state = NORMAL_STATE;
	    break;
	  }
      }
    else if (state == SEEN_ESC_LBRAC_STATE)
      {
	handleEscapeBracket((char) ch);
      }
    else /* if (state == SEEN_ESC_RBRAC_STATE) */
      {
	handleOperatingSystemCommand((char) ch);
      }
  }

  public synchronized void write (char[] data, int off, int len)
  {
    boolean move = marker.getOffset() == marker.buffer.getDot();
    while (len > 0)
      {
	if (state > NORMAL_STATE)
	  {
	    write1(data[off++]);
	    len--;
	  }
	else
	  {
	    int i;
	    if (state == NO_ESCAPES_STATE)
	      i = len;
	    else
	      {
		for (i = 0;  i < len;  i++)
		  {
		    char ch = data[off+i];
		    if (ch < ' ')
		      break;
		  }
	      }
	    if (i > 0)
	      {
		put(data, off, i);
		off += i;
		len -= i;
	      }
	    if (i < len)
	      {
		write1(data[off++]);
		len--;
	      }
	  }
      }
    if (move) marker.buffer.setDot(marker.getOffset());
  }

  public synchronized void flush()
  {
  }

  public synchronized void close()
  {
  }

  char[] buffer;
  int count;
  public void run()
  {
    write(buffer, 0, count);
  }
}
