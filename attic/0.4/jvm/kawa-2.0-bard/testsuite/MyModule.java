import gnu.expr.*;
public class MyModule implements Runnable
{
  public void run ()
  {
    Language language = Language.getDefaultLanguage();
    Object arg = Boolean.TRUE;
    language.defineFunction (new MyFunc ("my-func-t", arg));
  }
}

