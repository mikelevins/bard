package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;

/**
 * Implement the re-writer for the "begin" primitive.
 * @author	Per Bothner
 */

public class begin extends Syntax
{
  public static final begin begin = new begin();
  static { begin.setName("begin"); }

  public Expression rewrite (Object obj, Translator tr)
  {
    return tr.rewrite_body (obj);
  }

    @Override
    public void scanForm(Pair st, ScopeExp defs, Translator tr) {
        tr.scanBody(st.getCdr(), defs, false);
    }
}
