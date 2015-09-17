package gnu.kawa.functions;

import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import java.io.*;

/** A reference to a named feature/part of a specific object. */

class NamedPart extends ProcedureN
  implements HasSetter, Externalizable
{
  Object container;
  Object member;
  char kind;
  MethodProc methods;

  public NamedPart(Object container, Object member, char kind)
  {
    this.container = container;
    this.member = member;
    this.kind = kind;
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileNamedPart:validateNamedPart");
  }

  public NamedPart (Object container, String mname, char kind,
                    MethodProc methods)
  {
    this(container, mname, kind);
    this.methods = methods;
  }

  public int numArgs()
  {
    if (kind == 'I' || kind == 'C')
      return 0x1001;
    if (kind == 'D')
      return 0x1000;
    return 0xfffff000;
  }

  public void apply (CallContext ctx) throws Throwable
  {
    apply(ctx.getArgs(), ctx);
  }

  public void apply (Object[] args, CallContext ctx) throws Throwable
  {
    // Optimization, so that output from the
    // method is sent directly to ctx.consumer, rather than reified.
    if (kind == 'S')
      methods.checkN(args, ctx);
    else if (kind=='M')
      {
        int nargs = args.length;
        Object[] xargs = new Object[nargs+1];
        xargs[0] = container;
        System.arraycopy(args, 0, xargs, 1, nargs);
        methods.checkN(xargs, ctx);
      }
    else
      ctx.writeValue(this.applyN(args));
  }

  public Object applyN (Object[] args)
    throws Throwable
  {
    Object[] xargs;

    switch (kind)
      {
      case 'I':
        return kawa.standard.Scheme.instanceOf.apply2(args[0], container);
      case 'C':
        return gnu.kawa.functions.Convert.cast.apply2(container, args[0]);
      case 'N':
        xargs = new Object[args.length+1];
        xargs[0] = container;
        System.arraycopy(args, 0, xargs, 1, args.length);
        return Invoke.make.applyN(xargs);
      case 'S':
        return methods.applyN(args);
      case 'M':
        xargs = new Object[args.length+1];
        xargs[0] = container;
        System.arraycopy(args, 0, xargs, 1, args.length);
        return methods.applyN(xargs);
      case 'D':
        String fname = member.toString().substring(1);
        if (args.length == 0)
          return SlotGet.staticField((ClassType) container, fname);
        else
          return SlotGet.field(((Type) container).coerceFromObject(args[0]), fname);
      }
    throw new Error("unknown part "+member+" in "+container);
  }

  public Procedure getSetter()
  {
    if (kind == 'D')
      return new NamedPart.Setter(this);
    else
      throw new RuntimeException("procedure '"+getName()+ "' has no setter");
  }

  public void set0 (Object value) throws Throwable
  {
    switch (kind)
      {
      case 'D':
        String fname = member.toString().substring(1);
        SlotSet.setStaticField((ClassType) container, fname, value);
        return;
      default:
        throw new Error("invalid setter for "+this);
      }
  }

  public void set1 (Object object, Object value) throws Throwable
  {
    switch (kind)
      {
      case 'D':
        String fname = member.toString().substring(1);
        object = ((Type) container).coerceFromObject(object);
        SlotSet.setField(object, fname, value);
        return;
      default:
        throw new Error("invalid setter for "+this);
      }
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(container);
    out.writeObject(member);
    out.writeChar(kind);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    kind = in.readChar();
    container = (Procedure) in.readObject();
    member = (Procedure) in.readObject();
  }

  public static class Setter extends gnu.mapping.Setter
    implements Externalizable
  {
    public Setter (NamedPart getter)
    {
      super(getter);
      setProperty(Procedure.validateApplyKey,
                  "gnu.kawa.functions.CompileNamedPart:validateNamedPartSetter");
    }

    public int numArgs()
    {
      if (((NamedPart) getter).kind == 'D')
        return 0x2001;
      return 0xfffff000;
    }

    Procedure getGetter() { return getter; }

    public void writeExternal(ObjectOutput out) throws IOException
    {
      out.writeObject(getter);
    }

    public void readExternal(ObjectInput in)
      throws IOException, ClassNotFoundException
    {
      getter = (Procedure) in.readObject();
    }
  }
}
