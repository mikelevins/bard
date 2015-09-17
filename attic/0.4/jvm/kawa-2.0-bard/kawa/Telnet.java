package kawa;  // For now

/** Encapsulates the state of a telnet connection.
 * When run as an application, is a a minimal telnet client. */

// Some ideas from:  Flanagan:  "Java Examples in a Nutshell" (O'Reilly, 1997),
// example 9-8.

public class Telnet implements Runnable
{
  boolean isServer;
  final static int SE = 240; //    End of subnegotiation parameters.
  final static int NOP = 241; //   No operation.
  /*
      Data Mark           242    The data stream portion of a Synch.
                                 This should always be accompanied
                                 by a TCP Urgent notification.
 Break               243    NVT character BRK.*/
  final static int IP = 244; // Interrupt Process.
  final static int EOF = 236;  // End of file.

  /** Indicates that what follows is subnegotiation of the indicated option. */
  final static int SB =   250;

  /** Indicates the desire to begin performing, or confirmation that
      you are now performing, the indicated option. */
  public static final int WILL = 251;
  /** Indicates the refusal to perform,or continue performing, the
      indicated option. */
  public static final int WONT = 252;

  /** Indicates the request that the other party perform, or
      confirmation that you are expecting the other party to perform, the
      indicated option. */
  public static final int DO =   253;

  public static final int DONT = 254;
  /** Indicates the demand that the other party stop performing,
      or confirmation that you are no longer expecting the other party
      to perform, the indicated option. */

  /** The "Interpret As Command" prefix code. */
  final static int IAC = 255;

  // Various options.
  public static final int ECHO = 1;
  public static final int SUPPRESS_GO_AHEAD = 3;
  final static int TM = 6;  /* timing mark */
  final static int TTYPE = 24;  /* terminal type */
  final static int NAWS = 31; /* window size */
  final static int LINEMODE = 34;

  /* DEBUGGING:
  public static String toString(int code)
  {
    switch (code)
      {
      case DO:  return "DO";
      case DONT:  return "DONT";
      case WILL:  return "WILL";
      case WONT:  return "WONT";
      case ECHO:  return "ECHO";
      case LINEMODE: return "LINEMODE";
      case TTYPE: return "TTYPE";
      case NAWS:  return "NAWS";
      case SUPPRESS_GO_AHEAD: return "SUPPRESS_GO_AHEAD";
      default: return Integer.toString(code);
      }
  }
  */

  public short windowHeight, windowWidth;
  public byte[] terminalType;
  final byte preferredLineMode = 3; // EDIT+TRAPSIG

  java.io.InputStream sin;
  java.io.OutputStream sout;

  TelnetInputStream in;
  TelnetOutputStream out;

  public TelnetInputStream getInputStream()
  {
    return in;
  }

  public TelnetOutputStream getOutputStream()
  {
    return out;
  }

  /** Used to store the the current state of negotiation of telnet options.
   * For example, for option LINEMODE (34), (telnet_options_state[34] & 7)
   * is the state of the option on this side, and
   * ((telnet_options_state[34] >> 3) & 7) is the state on the other side.
   * The 3 bits for each side can be any of OPTION_NO though OPTION_YES.
   * The option is only enabled if the value is OPTION_YES.
   * See RFC 1143. */
  final byte[] optionsState = new byte[256];

  /** The option is disabled, and no negotiating is in progress. */
  final static int OPTION_NO = 0;

  /** We sent out DONT/WONT and are waiting for confirming WONT/DONT. */
  final static int OPTION_WANTNO = 1;

  /** Like WANTNO, but we changed our mind. */
  final static int OPTION_WANTNO_OPPOSITE = 2;

  /** We sent out DO/WILL and are waiting for confirming WILL/DO. */
  final static int OPTION_WANTYES = 3;

  /** Like WANTYES, but we changed our mind. */
  final static int OPTION_WANTYES_OPPOSITE = 4;

  /** The option is enabled, and no negotiating is in progress. */
  final static int OPTION_YES = 5;

  /** Actually (try to) change the state for an option.
   * Return false is we don't know how or don't want to.
   * command is DO if we're enabling on this side;
   * DONT if we're disabling on this side;
   * WILL if we're enabling for the other side;
   * WONT if we're disabling for the other side.
   *
   * You should not call this function directly.
   * Instead, call request to send a request to the other side
   * (but with DO/WILL and DONT/WONT switched).  Then, when
   * confirmation comes back, it is handled by the handle method, which
   * calls change.
   * The optionsState array may not have been updated yet.
   */
  boolean change (int command, int option)
  {
    if (option == TM)
      return true;
    if (isServer && option == NAWS)
      return true;
    if (isServer && command == WILL && option == LINEMODE)
      {
	byte[] buf = new byte[2];
	buf[0] = 1;  // MODE
	buf[1] = preferredLineMode;
	try
	  {
	    out.writeSubCommand(LINEMODE, buf);
	  }
	catch (java.io.IOException ex)
	  {
	    // Ignore it - I guess we'll do without.
	  }
        return true;
      }
    if (isServer && command == WILL && option == TTYPE)
      {
	byte[] buf = new byte[1];
	buf[0] = 1;  // Request SEND terminal-type.
	try
	  {
	    out.writeSubCommand(option, buf);
	  }
	catch (java.io.IOException ex)
	  {
	    // Ignore it - I guess we'll do without.
	  }
        return true;
      }
    if (! isServer && option == ECHO)
      {
	if (command == DO)
	  return false;
	if (command == WILL)
	  return true;
      }
    return false;
  }

  /** Handle a sub-command (SB-sequence) that we received. */

  public void subCommand (byte[] buf, int off, int len)
  {
    int command = buf[off];
    switch (command)
      {
      case NAWS:
	if (len == 5)
	  {
	    windowWidth = (short) ((buf[1] << 8) + (buf[2] & 0xFF));
	    windowHeight = (short) ((buf[3] << 8) + (buf[4] & 0xFF));
	    /*
	    System.err.println("Window size: w:"
			       +windowWidth+"*h:"+windowHeight);
	    */
	    return;
	  }
	break;
      case TTYPE:
	byte[] type = new byte[len-1];
	System.arraycopy(buf, 1, type, 0, len-1);
	terminalType = type;
	 System.err.println("terminal type: '"+new String(type)+"'");
	return;
      case LINEMODE:
	///*
	System.err.println("SBCommand LINEMODE "+buf[1]+" len:"+len);
	if (buf[1] == 3) // SLC
	  {
	    for (int i = 2;  i+2 < len;  i += 3)
	      {
		System.err.println("  "+buf[i]+","+buf[i+1]+","+buf[i+2]);
	      }
	    return;
	  }
	//*/
	break;
      }
  }

  /** Handle a request from the other side.
   * Command is one of DO, DONT, WILL, WONT. */
  void handle (int command, int option) throws java.io.IOException
  {
    // True if the other side wants to change itself I.e. we got WILL/WONT);
    // false if it wants us to change (i.e. we got DO/DONT).
    boolean otherSide = command < DO;

    // True if DO or WILL; false if DONT or WONT.
    boolean wantOn = (command & 1) != 0;
    byte state = optionsState[option];
    // System.err.println("telnet handle "+toString(command)+", "+toString(option));
    if (otherSide)
      state >>= 3;
    switch ((state >> 3) & 7)
      {
      case OPTION_YES:
	if (wantOn)
	  return; // Nothing to do.
	// Got a DONT or WONT.  Disable without arguing.
	state = OPTION_NO;
	change(command, option);
	out.writeCommand(otherSide ? DONT : WONT, option);
	break;
      case OPTION_NO:
	if (! wantOn)
	  return; // Nothing to do.
	if (change (command, option))
	  {
	    state = OPTION_YES;
	    out.writeCommand(otherSide ? DO : WILL, option); // Confirm.
	  }
	else
	  {
	    out.writeCommand(otherSide ? Telnet.DONT : Telnet.WONT,
			     option);
	  }
	break;
      case OPTION_WANTNO:
	state = OPTION_NO;
	break;
      case OPTION_WANTNO_OPPOSITE:
	// if (goalState) Error:  DONT/WONT answered by WILL/DO.
	// Maybe some packets crossed in the mail.
	// Presumably the other side will take our original
	// request as de-conformation.  In any case:
	state = OPTION_WANTYES;
	out.writeCommand(otherSide ? Telnet.DO : Telnet.WILL,
			 option);
	break;
      case OPTION_WANTYES:
	if (wantOn)
	  {
	    state = OPTION_YES;
	    change (command, option);
	  }
	else
	  state = OPTION_NO; // Declined.
	break;
      case OPTION_WANTYES_OPPOSITE:
	if (wantOn)
	  {
	    state = OPTION_WANTNO;
	    out.writeCommand(otherSide ? DONT : WONT, option);
	  }
	else
	  {
	    state = OPTION_NO;
	  }
	break;
      }
    if (otherSide)
      state = (byte) ((optionsState[option] & 0xC7) | (state << 3));
    else
      state = (byte) ((optionsState[option] & 0xF8) | state);
    optionsState[option] = state;
  }

  /** Request (from this side) a new option state.
   * Command is one of DO, DONT, WILL, WONT. */
  public void request (int command, int option) throws java.io.IOException
  {
    // System.err.println("telnet request "+toString(command)+", "+toString(option));
    // True if we want other side to change,
    // false if we want this side to change.
    boolean otherSide = command >= DO;

    // True for DO, WILL; false for DONT or WONT.
    boolean wantOn = (command & 1) != 0;

    byte state = optionsState[option];
    if (otherSide)
      state >>= 3;

    switch (state & 7)
      {
      case OPTION_NO:
	if (wantOn)
	  {
	    state = OPTION_WANTYES;
	    out.writeCommand(command, option);
	  }
	// else:  Redundant - already disabled.
	break;
      case OPTION_YES:
	if (! wantOn)
	  {
	    state = OPTION_WANTNO;
	    out.writeCommand(command, option);
	  }
	// else:  Redundant - already enabled.
	break;
      case OPTION_WANTNO:
	if (wantOn)
	  state = OPTION_WANTNO_OPPOSITE;
	// else:  Error/redundant - already want to disable.
	break;
      case OPTION_WANTNO_OPPOSITE:
	if (! wantOn)
	  state = OPTION_WANTNO;
	// else:  Error/redundant - already want to enable
	break;
      case OPTION_WANTYES:
	if (! wantOn)
	  state = OPTION_WANTYES_OPPOSITE;
	// else:  Error/redundant - already want to disable.
      case OPTION_WANTYES_OPPOSITE:
	if (wantOn)
	  state = OPTION_WANTYES;
	// else:  Error/redundant - already want to enable
	break;
      }

    if (otherSide)
      state = (byte) ((optionsState[option] & 0xC7) | (state << 3));
    else
      state = (byte) ((optionsState[option] & 0xF8) | state);
    optionsState[option] = state;      
  }

  static void usage ()
  {
    System.err.println("Usage:  [java] kawa.Telnet HOST [PORT#]");
    System.exit(-1);
  }

  public static void main (String[] args)
  {
    if (args.length == 0)
      usage();
    String host = args[0];
    int port = 23;
    if (args.length > 1)
      {
	port = Integer.parseInt(args[1]);
      }
    try
      {
	java.net.Socket socket = new java.net.Socket(host, port);
	Telnet telnet = new Telnet(socket, false);
	TelnetOutputStream tout = telnet.getOutputStream();
	Thread t = new Thread(telnet);

	t.setPriority(Thread.currentThread().getPriority() + 1);
	t.start();

	byte[] buffer = new byte[1024];
	for (;;)
	  {
	    int ch = System.in.read();
	    if (ch < 0)
	      break; // send EOF FIXME
	    buffer[0] = (byte) ch;
	    int avail = System.in.available();
	    if (avail > 0)
	      {
		if (avail > buffer.length-1)
		  avail = buffer.length - 1;
		avail = System.in.read(buffer, 1, avail);
	      }
	    tout.write(buffer, 0, avail+1);
	  }
	t.stop();
      }
    catch (Exception ex)
      {
	System.err.println(ex);
      }
  }

  public Telnet (java.net.Socket socket, boolean isServer)
    throws java.io.IOException
  {
    sin = socket.getInputStream();
    sout = socket.getOutputStream();
    out = new TelnetOutputStream(sout);
    in = new TelnetInputStream(sin, this);
    this.isServer = isServer;
  }

  public void run ()
  {
    try
      {
	TelnetInputStream tin = getInputStream();
	byte[] buffer = new byte[1024];
	for (;;)
	  {
	    int ch = tin.read();
	    if (ch < 0)
	      break; // ??? FIXME
	    buffer[0] = (byte) ch;
	    int avail = tin.available();
	    if (avail > 0)
	      {
		if (avail > buffer.length-1)
		  avail = buffer.length - 1;
		avail = tin.read(buffer, 1, avail);
	      }
	    System.out.write(buffer, 0, avail+1);
	  }
      }
    catch (java.io.IOException ex)
      {
	System.err.println(ex);
	System.exit(-1);
      }
  }
}
