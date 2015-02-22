package gnu.jemacs.buffer;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.jemacs.lang.ELisp;
import gnu.math.IntNum;

public class Command
{
  static Object resolveSymbol(Object command)
  {
    int count = 100;
    for (;;)
      {
	if (command instanceof String)
	  command = Namespace.getDefaultSymbol((String) command);
	if (command instanceof Symbol)
	  {
	    Symbol sym = (Symbol) command;
	    Environment env = Environment.getCurrent();
	    command = env.getFunction(sym, null);
	    if (command == null)
	      command = env.get(sym, null);
	  }
	else
	  return command;
	if (--count < 0)
	  throw new Error("circular binding for "+command);
      }
  }


  /** Perform a given command as appropriate for its class. */
  public static void perform(Object command)
  {
    perform(command, EWindow.getSelected());
  }

  public static void perform(Object command, EWindow window)
  {
    window.handleCommand(command);
  }

  public static Object[] processInteractionString (String str)
  {
    int len = str.length();
    int i = 0;
    int start = 0;
    int argCount = 0;
    Buffer buffer = Buffer.getCurrent();
    while (i < len)
      {
	char ch = str.charAt(i++);
	switch (ch)
	  {
	  case '\n':
	    continue;
	  case '@':
	    if (start == i-1) start = i;
	    // FIXME: select-window
	    break;
	  case '*':
	    if (start == i-1) start = i;
	    // FIXME:  check readonly
	    break;
	  case '_':
	    if (start == i-1) start = i;
	    // FIXME:  region stays:
	    break;
	  case 'r':
	    argCount++;
	    // ... fall through ...
	  default:
	    argCount++;
	    while (i < len)
	      {
		ch = str.charAt(i++);
		if (ch == '\n')
		  break;
	      }
	  }
      }
    Object[] args = new Object[argCount];
    int argIndex = 0;
    i = start;
    while (i < len)
      {
	char ch = str.charAt(i++);
	int promptStart = i;
	int promptLength;
	for (;;)
	  {
	    if (i >= len)
	      {
		promptLength = i - promptStart;
		break;
	      }
	    char c = str.charAt(i++);
	    if (c == '\n')
	      {
		promptLength = i - 1 - promptStart;
		break;
	      }
	  }
	switch (ch)
	  {
	  case 'P':
	    args[argIndex++] = ELisp.FALSE; // FIXME
	    break;
	  case 'p':
	    args[argIndex++] = IntNum.one(); // FIXME
	    break;
          case 'r':
            int mark = buffer.checkMark() + 1;
            int point = buffer.getPoint();
            if (mark <= point)
              {
                args[argIndex++] = IntNum.make(mark);
                args[argIndex++] = IntNum.make(point);
              }
            else
              {
                args[argIndex++] = IntNum.make(point);
                args[argIndex++] = IntNum.make(mark);
              }
            break;
          case 'F':  // FIXME
          case 's':
          case 'S':
            String answer =
              EFrame.selectedFrame.ask(str.substring(promptStart,
						   promptStart+promptLength));
            args[argIndex++]
              = (ch == 'S' ? (Object) answer.intern()
                 : (Object) new FString(answer));
            break;
	  default:
	    System.err.println("un-implemented interactive prompt:"+ch);
	    args[argIndex++] = ELisp.FALSE;
	  }
      }
    return args;
  }
}
