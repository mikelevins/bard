// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.kawa.io.CharArrayOutPort;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;

/** A 1-argument Procedure that takes a value and return output in XML syntax.
 */

public class OutputAsXML extends Procedure1
{
  public int numArgs() { return 0x1001; }

  public Object apply1 (Object arg)
  {
    CharArrayOutPort port = new CharArrayOutPort();
    XMLPrinter out = new XMLPrinter(port);
    out.writeObject(arg);
    out.flush();
    return new FString(port.toCharArray());
  }
}
