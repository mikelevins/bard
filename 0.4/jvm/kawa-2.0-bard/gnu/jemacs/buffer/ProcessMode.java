// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.buffer;
import gnu.mapping.*;
import java.io.*;

public class ProcessMode extends Mode
{
  protected Writer toInferior;

  protected Marker processMark;

  protected boolean lineMode = false;

  static Procedure enterAction = new gnu.expr.PrimProcedure(gnu.bytecode.ClassType.make("gnu.jemacs.buffer.ProcessMode").getDeclaredMethod("enterAction", 0));

  public static EKeymap modeMap = new EKeymap("process");
  static
  {
    modeMap.defineKey("\n", enterAction);
    modeMap.defineKey("\r", enterAction);
    modeMap.defineKey("enter", enterAction);
    modeMap.defineKey("return", enterAction);
    Object insert = new ProcessInsertCommand();
    //    modeMap.setDefaultBinding(insert);
    modeMap.set(' ', 126, insert);
    modeMap.set(128, 255, insert);
  }

  public Marker getProcessMark ()
  {
    return processMark;
  }

  public static void enterAction()
  {
    Buffer buffer = Buffer.getCurrent();
    ProcessMode pmode = getProcessMode(buffer);
    pmode.enter();
  }

  public void enter()
  {
    try
      {
	if (lineMode)
	  {
	    buffer.insertChar('\n', 1, null);
	    int pos = buffer.getDot();
	    int markPos = processMark.getOffset();

	    buffer.writeTo(markPos, pos - markPos, toInferior);
	    processMark.setDot(pos);
	  }
	else
	  toInferior.write('\r');
	toInferior.flush();
      }
    catch (Exception ex)
      {
        throw new WrappedException(ex);
      }
  }

  public static ProcessMode getProcessMode(Buffer buffer)
  {
    for (Mode mode = buffer.modes;  ;  mode = mode.next)
      {
	if (mode == null)
	  Signal.error("not in process mode");
	if (mode instanceof ProcessMode)
	  return (ProcessMode) mode;
      }
  }

  public void writeToInferior(gnu.lists.CharSeq str)
    throws java.io.IOException
  {
    str.writeTo(toInferior);
    toInferior.flush();
  }

  public void selfInsert()
  {
    EWindow window = EWindow.getSelected();
    insert((char) window.pendingKeys[window.pendingLength], 1);
  }

  public void insert(char ch, int count)
  {
    if (! lineMode)
      {
	try
	  {
	    while (--count >= 0)
	      toInferior.write(ch);
	    toInferior.flush();
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException(ex);
	  }
      }
    else
      buffer.insertChar(ch, count);
  }

  /**
   * 
   * @param doRun
   */
  public void invoke(Runnable doRun) 
  {
    buffer.invoke(doRun);
  }
}

class ProcessInsertCommand extends Procedure0
{
  public Object getProperty (Object key, Object defaultValue)
  {
    if (key == "emacs-interactive")
      return "*";
    return super.getProperty(key, defaultValue);
  }

  public Object apply0 ()
  {
    EWindow window = EWindow.getSelected();
    Buffer buffer = window.buffer;
    ProcessMode pmode = ProcessMode.getProcessMode(buffer);
    pmode.insert((char) window.pendingKeys[window.pendingLength], 1);
    return Values.empty;
  }
}
