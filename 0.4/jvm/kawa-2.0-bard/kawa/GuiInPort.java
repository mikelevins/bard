package kawa;

import java.io.Reader;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.io.TtyInPort;
import gnu.mapping.*;

/** A TtyInPort that reads from a ReplPane.
  */

class GuiInPort extends TtyInPort
{
  ReplDocument document;

  public GuiInPort (Reader in, Path path, OutPort tie, ReplDocument document)
  {
    super (in, path, tie);
    this.document = document;
  }

  public void emitPrompt (String prompt) throws java.io.IOException
  {
    document.write(prompt, ReplDocument.promptStyle);
  }
}
