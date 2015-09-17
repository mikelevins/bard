package gnu.expr;

public interface Inlineable
{
  public void compile (ApplyExp exp, Compilation comp, Target target);
}

