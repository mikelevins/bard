// Copyright (c) 2010  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ../../../COPYING.

package gnu.kawa.lispexpr;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.Namespace;
import kawa.lang.*;

public class ResolveNamespace extends Syntax
{
  public static final ResolveNamespace resolveNamespace =
    new ResolveNamespace("$resolve-namespace$", false);
  public static final ResolveNamespace resolveQName =
    new ResolveNamespace("$resolve-qname$", true);

  boolean resolvingQName;

  public ResolveNamespace(String name, boolean resolvingQName)
  {
    super(name);
    this.resolvingQName = resolvingQName;
  }

    public Expression rewriteForm (Pair form, Translator tr) {
        Object cdr = form.getCdr();
        String local;
        if (resolvingQName) {
            // look for name [prefix]
            Pair pair = (Pair) cdr;
            local = pair.getCar().toString();
            cdr = pair.getCdr();
        }
        else
            local = null;
        if (! (cdr instanceof Pair))
            cdr = LList.list1(ReaderXmlElement.defaultElementNamespaceSymbol);
        Pair pair = (Pair) cdr;
        Expression prefix = tr.rewrite_car(pair, false);
        Namespace namespace = tr.namespaceResolvePrefix(prefix);
        if (namespace == null) {
            String pstr = pair.getCar().toString();
            if (pstr == ReaderXmlElement.DEFAULT_ELEMENT_NAMESPACE)
                namespace = Namespace.EmptyNamespace;
            else {
                Object savePos = tr.pushPositionOf(pair);
                tr.error('e', "unknown namespace prefix "+pstr);
                tr.popPositionOf(savePos);
                namespace = Namespace.valueOf(pstr, pstr);
            }
        }
        if (resolvingQName)
            return new QuoteExp(namespace.getSymbol(local));
        else
            return new QuoteExp(namespace);
    }
}
