package gnu.kawa.lispexpr;
import kawa.lang.*;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.bytecode.ClassType;
import gnu.kawa.xml.XmlNamespace;

public class DefineNamespace extends Syntax
{
  private boolean makePrivate;
  private boolean makeXML;
  
  public static final DefineNamespace define_namespace
    = new DefineNamespace();
  public static final DefineNamespace define_private_namespace
    = new DefineNamespace();
  public static final DefineNamespace define_xml_namespace
    = new DefineNamespace();
  static {
    define_namespace.setName("define-namespace");
    define_private_namespace.setName("define-private-namespace");
    define_private_namespace.makePrivate = true;
    define_xml_namespace.setName("define-xml-namespace");
    define_xml_namespace.makeXML = true;
  }

  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    Pair p1, p2;
    if (! (st.getCdr() instanceof Pair)
        || ! ((p1 = (Pair) st.getCdr()).getCar() instanceof Symbol)
	|| ! (p1.getCdr() instanceof Pair)
	|| (p2 = (Pair) p1.getCdr()).getCdr() != LList.Empty)
      {
	tr.error('e', "invalid syntax for define-namespace");
	return false;
      }
    Symbol name = (Symbol) p1.getCar();
    Declaration decl = defs.getDefine(name, 'w', tr);
    tr.push(decl);
    decl.setFlag(Declaration.IS_CONSTANT|Declaration.IS_NAMESPACE_PREFIX
                 |Declaration.IS_SINGLE_VALUE);
    if (makePrivate)
      {
        decl.setFlag(Declaration.PRIVATE_SPECIFIED);
        decl.setPrivate(true);
      }
    else if (defs instanceof ModuleExp)
      decl.setCanRead(true);
    Translator.setLine(decl, p1);
    Expression value;
    String literal = null;
    if
      /* #ifdef use:java.lang.CharSequence */
      (p2.getCar() instanceof CharSequence) 
      /* #else */
      // (p2.getCar() instanceof CharSeq || p2.getCar() instanceof String) 
      /* #endif */
      {
        literal = p2.getCar().toString();
        Namespace namespace;
        String prefix = name.getName();
        if (literal.startsWith("class:"))
          {
            String cname = literal.substring(6);
            namespace
              = ClassNamespace.getInstance(literal, ClassType.make(cname));
            decl.setType(ClassType.make("gnu.kawa.lispexpr.ClassNamespace"));
          }
        else if (makeXML)
          {
            namespace = XmlNamespace.getInstance(prefix, literal);
            decl.setType(ClassType.make("gnu.kawa.xml.XmlNamespace"));
          }
        else
          {
            namespace = Namespace.valueOf(literal, prefix);
            decl.setType(ClassType.make("gnu.mapping.Namespace"));
          }
        value = new QuoteExp(namespace);
	decl.setFlag(Declaration.TYPE_SPECIFIED);
       }
    else
      value = tr.rewrite_car (p2, false);
    decl.noteValue(value);
    tr.pushForm(SetExp.makeDefinition(decl, value));
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return tr.syntaxError ("define-namespace is only allowed in a <body>");
  }

  public static final String XML_NAMESPACE_MAGIC = "&xml&";
}
