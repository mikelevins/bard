package gnu.expr;
import javax.script.*;
import java.io.*;
import gnu.mapping.*;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.text.SourceError;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;

public class KawaScriptEngine extends AbstractScriptEngine
  implements Compilable //, Invocable
{
  AbstractScriptEngineFactory factory;

  public KawaScriptEngine(AbstractScriptEngineFactory factory)
  {
    this.factory = factory;
    context.setBindings(createBindings(), ScriptContext.ENGINE_SCOPE);
    context.setReader(InPort.inDefault());
    context.setWriter(OutPort.outDefault());
    context.setErrorWriter(OutPort.errDefault());
 }

  public AbstractScriptEngineFactory getFactory()
  {
    return factory;
  }

  public Bindings createBindings()
  {
    SimpleEnvironment env = new SimpleEnvironment();
    Bindings bindings = new KawaScriptBindings(env);
    return bindings;
  }

  public Object eval(Reader in, ScriptContext context)
    throws ScriptException
  {
    return eval(in instanceof InPort ? (InPort) in : new InPort(in), context);
  }

  public Object eval(String string, ScriptContext context)
    throws ScriptException
  {
    return eval(new CharArrayInPort(string), context);
  }

  public Object eval(InPort in, ScriptContext context)
    throws ScriptException
  {
    KawaCompiledScript compiled = compile(in, context);
    return compiled.eval(context);
  }

  public KawaCompiledScript compile(String string)
    throws ScriptException
  {
    return compile(new CharArrayInPort(string), getContext());
  }

  public KawaCompiledScript compile(Reader in)
    throws ScriptException
  {
    return compile(in instanceof InPort ? (InPort) in : new InPort(in), getContext());
  }

  public KawaCompiledScript compile(InPort port, ScriptContext context)
    throws ScriptException
  {
    SourceMessages messages = new SourceMessages();
    try
      {
        return compile(port, context, messages);
      }
    catch (SyntaxException ex)
      {
        messages = ex.getMessages();  // Presumably a no-op.
        SourceError err = messages.getErrors();
        if (messages.seenErrors())
          {
            while (err.severity == 'w' && err.next != null)
              err = err.next;
          }
        throw new ScriptException(err.message, err.filename,
                                  err.line, err.column);
      }
    catch (Exception ex)
      {
        throw new ScriptException(ex);
      }
  }

  public KawaCompiledScript compile(InPort port, ScriptContext context, SourceMessages messages)
    throws SyntaxException, IOException
  {
    Language saveLang = Language.setSaveCurrent(factory.language);
    Environment env = factory.getEnvironment(context);
    Environment saveEnv = Environment.setSaveCurrent(env);
    try
      {
        Compilation comp =
            factory.language.parse(port, messages,
                                   Language.PARSE_IMMEDIATE|Language.PARSE_INTERACTIVE_MODULE);
        if (messages.seenErrors())
          throw new SyntaxException(messages);
        ModuleExp mexp = comp.getModule();
        String filename = (String) get(ScriptEngine.FILENAME);
        java.net.URL url;
        if (filename != null)
          url = Path.toURL(filename);
        else if (port instanceof CharArrayInPort)
          url = null;
        else
          {
            Path portpath = port.getPath();
            url = portpath == null ? null : portpath.toURL();
          }
        Writer errorWriter = context.getErrorWriter();
        OutPort errorPort
          = (errorWriter instanceof OutPort ? (OutPort) errorWriter
             : new OutPort(errorWriter));
        Object inst = mexp.evalModule1(env, comp, url, errorPort);
        return new KawaCompiledScript(this, mexp, inst);
      }
    finally
      {
	Language.restoreCurrent(saveLang);
        Environment.restoreCurrent(saveEnv);
      }
  }
}

class KawaCompiledScript extends CompiledScript
{
  KawaScriptEngine engine;
  CompiledModule cmodule;

  public KawaCompiledScript(KawaScriptEngine engine,
                            ModuleExp mexp, Object cookie)
  {
    this.engine = engine;
    this.cmodule = new CompiledModule(mexp, cookie, engine.factory.language);
  }

  public Object eval(ScriptContext context) throws ScriptException
  {
    try
      {
        return cmodule.evalToResultValue(engine.factory.getEnvironment(context),
                                         CallContext.getInstance());
      }
    catch (Throwable ex)
      {
        if (ex instanceof Exception)
          throw new ScriptException((Exception) ex);
        else if (ex instanceof Error)
          throw (Error) ex;
        else
          throw new RuntimeException(ex);
      }
  }

  public KawaScriptEngine getEngine()
  {
    return engine;
  }
}
