package gnu.jemacs.lang;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;

public class defgroup extends Syntax
{
  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    // ignore, for now.  FIXME!
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
