// Copyright (c) 2010  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ../../../COPYING.

package gnu.kawa.lispexpr;
import gnu.expr.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import kawa.lang.*;

/** A Syntax transformer for a XML constructor.
 * This is needed to put namespace declarations in the lexical scope.
 */

public class MakeXmlElement extends Syntax
{
  public static final MakeXmlElement makeXml = new MakeXmlElement();
  static { makeXml.setName("$make-xml$"); }

  static final ClassType typeNamespace =
    ClassType.make("gnu.mapping.Namespace");

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair pair1 = (Pair) form.getCdr();
    Object namespaceList = pair1.getCar();
    Object obj = pair1.getCdr();
    boolean nsSeen = false;
    NamespaceBinding saveBindings = tr.xmlElementNamespaces;
    NamespaceBinding nsBindings = saveBindings;
    while (namespaceList instanceof Pair)
      {
        if (! nsSeen)
          {
            tr.letStart();
            nsSeen = true;
          }
        Pair namespacePair = (Pair) namespaceList;
        Pair namespaceNode = (Pair) namespacePair.getCar();
        String nsPrefix = namespaceNode.getCar().toString();
        nsPrefix = nsPrefix.length() == 0 ? null : nsPrefix.intern();
        Object valueList = namespaceNode.getCdr();
        StringBuilder sbuf = new StringBuilder();
        while (valueList instanceof Pair)
          {
            Pair valuePair = (Pair) valueList;
            Object valueForm = valuePair.getCar();
            Object value;
            if (LList.listLength(valueForm, false) == 2
                && valueForm instanceof Pair
                && ((Pair) valueForm).getCar() == ReaderXmlElement.xmlTextSymbol)
              {
                value = ((Pair) ((Pair) valueForm).getCdr()).getCar();
              }
            else
              {
                Expression valueExp = tr.rewrite_car(valuePair, false);
                value = valueExp.valueIfConstant();
              }
            if (value == null)
              {
                Object savePos = tr.pushPositionOf(valuePair);
                tr.error('e', "namespace URI must be literal");
                tr.popPositionOf(savePos);
              }
            else
              sbuf.append(value);
            valueList = valuePair.getCdr();
          }
        String nsUri = sbuf.toString().intern();
        // FIXME: check for duplicate prefix
        // FIXME: check that uri not same as predefined xml namespace
        // FIXME: check that prefix isn't "xml" or "xmlns"
        nsBindings
          = new NamespaceBinding(nsPrefix,
                                 nsUri == "" ? null : nsUri,
                                 nsBindings);
        Namespace namespace;
        if (nsPrefix == null)
          {
            namespace = Namespace.valueOf(nsUri);
            nsPrefix = ReaderXmlElement.DEFAULT_ELEMENT_NAMESPACE;
          }
        else
          {
            namespace = XmlNamespace.getInstance(nsPrefix, nsUri);
          }
        Symbol nsSymbol = Namespace.EmptyNamespace.getSymbol(nsPrefix);
        Declaration decl = tr.letVariable(nsSymbol, typeNamespace,
                                          new QuoteExp(namespace));
        decl.setFlag(Declaration.IS_CONSTANT|Declaration.IS_NAMESPACE_PREFIX|Declaration.TYPE_SPECIFIED);
        // FIXME:  Translator.setLine(decl, p1);
        namespaceList = namespacePair.getCdr();
      }
    // if (namespaceList != LList.Empty) ERROR;
    MakeElement mkElement = new MakeElement();
    mkElement.setNamespaceNodes(nsBindings);
    mkElement.setStringIsText(true);
    tr.xmlElementNamespaces = nsBindings;
    try
      {
        if (nsSeen)
          tr.letEnter();
        Expression result = tr.rewrite(Translator.makePair(form, mkElement, obj));
        return nsSeen ? tr.letDone(result) : result;
      }
    finally
      {
        tr.xmlElementNamespaces = saveBindings;
      }
  }
}
