package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.Stack;

/**
 * The Syntax transformer that re-writes the Scheme "let" primitive.
 * This only handles standard "unnamed" let.
 * The let macro in ../lib/let.scm handles named let as well.
 * @author	Per Bothner
 */

public class let extends Syntax
{
  public static final let let = new let("let", false);
  
  /**
   * Used for constructs such as FLET, where we intend to set a function binding
   * rather than an ordinary binding.
   */
  protected boolean settingProcedures;
  
  public let(String name, boolean settingProcedures) {
    this.setName(name);
    this.settingProcedures = settingProcedures;
  }

  @Override
  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing " + getName() + " arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.getCar();
    Object body = pair.getCdr();
    int decl_count = Translator.listLength(bindings);
    if (decl_count < 0)
      return tr.syntaxError("bindings not a proper list");
    
    Expression[] inits = new Expression[decl_count];
    Declaration[] decls = new Declaration[decl_count];
    // Used to check for duplicate definitions.
    SimpleEnvironment dupenv = new SimpleEnvironment();
    Stack renamedAliases = null;
    int renamedAliasesCount = 0;
    SyntaxForm syntaxRest = null;
    for (int i = 0; i < decl_count; i++)
      {
	while (bindings instanceof SyntaxForm)
	  {
	    syntaxRest = (SyntaxForm) bindings;
	    bindings = syntaxRest.getDatum();
	    // The SyntaxForm "surrounds" both the current binding (the car),
	    // as well as the cdr - i.e. the remaining bindings.
	  }
	Pair bind_pair = (Pair) bindings;
	Object bind_pair_car = bind_pair.getCar();
	SyntaxForm syntax = syntaxRest;
	if (bind_pair_car instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) bind_pair_car;
	    bind_pair_car = syntax.getDatum();
	  }
	if (! (bind_pair_car instanceof Pair))
	  return tr.syntaxError (getName() + 
                                 " binding is not a pair:"+bind_pair_car);
	Pair binding = (Pair) bind_pair_car;
	Object name = binding.getCar();
	TemplateScope templateScope;
	if (name instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) name;
	    name = sf.getDatum();
	    templateScope = sf.getScope();
	  }
	else
	  templateScope = syntax == null ? null : syntax.getScope();
        name = tr.namespaceResolve(name);
	if (! (name instanceof Symbol))
	  return tr.syntaxError("variable "+name+" in " + getName() + 
                                " binding is not a symbol: "+obj);
	Declaration decl = new Declaration(name);
        Translator.setLine(decl, binding);
        Symbol sym = (Symbol) name;
        Object old = dupenv.get(sym, templateScope, null);
        if (old != null)
          ScopeExp.duplicateDeclarationError((Declaration) old, decl, tr);
        dupenv.put(sym, templateScope, decl);
        decls[i] = decl;
        decl.setFlag(Declaration.IS_SINGLE_VALUE);
        maybeSetProcedure(decl);
	if (templateScope != null)
	  {
	    Declaration alias = tr.makeRenamedAlias(decl, templateScope);
	    if (renamedAliases == null)
	      renamedAliases = new Stack();
	    renamedAliases.push(alias);
	    renamedAliasesCount++;
	  }

	Object binding_cdr = binding.getCdr();
	while (binding_cdr instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) binding_cdr;
	    binding_cdr = syntax.getDatum();
	  }
	if (! (binding_cdr instanceof Pair))
	  return tr.syntaxError(getName() + " has no value for '"+name+"'");
	binding = (Pair) binding_cdr;
	binding_cdr = binding.getCdr();
	Pair init;
	while (binding_cdr instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) binding_cdr;
	    binding_cdr = syntax.getDatum();
	  }
	if (tr.matches(binding.getCar(), "::"))
	  {
	    if (! (binding_cdr instanceof Pair)
		|| (binding = (Pair) binding_cdr).getCdr() == LList.Empty)
	      return tr.syntaxError("missing type after '::' in " + getName());
	    binding_cdr = binding.getCdr();
	    while (binding_cdr instanceof SyntaxForm)
	      {
		syntax = (SyntaxForm) binding_cdr;
		binding_cdr = syntax.getDatum();
	      }
	  }
	if (binding_cdr == LList.Empty)
	  {
	    init = binding;
	  }
	else if (binding_cdr instanceof Pair)
	  {
            decl.setType(tr.exp2Type(binding, null, syntax));
	    decl.setFlag(Declaration.TYPE_SPECIFIED);
	    init = (Pair) binding_cdr;
	  }
	else
	  return tr.syntaxError(getName() + " binding for '" + getName() +
                                "' is improper list");
        inits[i] = tr.rewrite_car (init, syntax);
	if (init.getCdr() != LList.Empty)
	  return tr.syntaxError("junk after declaration of "+getName());
	bindings = bind_pair.getCdr();
      }

    for (int i = renamedAliasesCount;  --i >= 0; )
      tr.pushRenamedAlias((Declaration) renamedAliases.pop());

    tr.letStart();
    for (int i = 0; i < decl_count;  i++)
      tr.letVariable(decls[i], inits[i]);
    tr.letEnter();
    LetExp let = tr.letDone(tr.rewrite_body(body));
    tr.popRenamedAlias(renamedAliasesCount);
    
    return let;
  }

  /**
   * Set the procedure flag of a declaration if binding a function property.
   * 
   * This is used for FLET .vs. LET distinction, where {@code settingProcedures}
   * is true for FLET, and false for LET.
   * 
   * @param decl The declaration to possibly set the {@code PROCEDURE} flag.
   */
  protected void maybeSetProcedure (Declaration decl)
  {
    if (settingProcedures)
      decl.setProcedureDecl(true); 
  }
}
