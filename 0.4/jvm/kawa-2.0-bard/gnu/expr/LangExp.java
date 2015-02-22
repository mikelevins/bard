// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.kawa.io.OutPort;
import gnu.mapping.*;

/** A language-specific expression. */

public class LangExp extends Expression
{
  Object hook;

  public Object getLangValue () { return hook; }
  public void setLangValue (Object value) { hook = value; }

  public LangExp () { }

  public LangExp (Object value) { this.hook = value; }

  protected boolean mustCompile () { return false; }

  public void print (OutPort out)
  {
    out.print("(LangExp ???)");
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitLangExp(this, d);
  }

  public void compile (Compilation comp, Target target)
  {
    throw new RuntimeException("compile called on LangExp");
  }
}
