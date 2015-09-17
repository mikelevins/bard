package gnu.commonlisp.lang;
import gnu.bytecode.ClassType;
import gnu.bytecode.CodeAttr;
import gnu.expr.*;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.text.SourceMessages;
import kawa.lang.Translator;

public class Lisp2Compilation extends Translator
{
  public Lisp2Compilation (Language language, SourceMessages messages, NameLookup lexical)
  {
    super(language, messages, lexical);
  }

  @Override
  public void emitPushBoolean(boolean value)
  {
    CodeAttr code = getCode();
    if (value)
      code.emitGetStatic(ClassType.make("gnu.commonlisp.lang.Lisp2").getDeclaredField("TRUE"));
    else
      code.emitGetStatic(Compilation.scmListType.getDeclaredField("Empty"));
  }
  
  /**
   * Process the DECLARE (if any) in a Common Lisp form.
   *
   * @param list The body of this expression.
   */
  @Override
  protected void rewriteBody (LList list)
  {
    if (list.isEmpty())
      return;
    // Declarations are always at the start of a body.
    Object head = ((Pair) list).getCar();
    // The body to rewrite into the new LET body, this is just what follows
    // the DECLARE form in list
    Object body = ((Pair) list).getCdr();
    
    if (head instanceof Pair && matches(((Pair) head).getCar(), "declare"))
    {
      Object decls = ((Pair) head).getCdr();
      Object vars;
      
      // Create a new lexical environment for this DECLARE
      letStart();

      while (decls != LList.Empty) {
        
        if (!( (decls instanceof Pair) && 
               (((Pair) decls).getCar() instanceof Pair) )) {
          errorWithPosition("Arguments to declare must be proper lists", decls);
          break;
        }
        
        Pair declItem = (Pair) ((Pair) decls).getCar();
        
        if (!(declItem.getCdr() instanceof Pair))
        {
          errorWithPosition("Bad declare syntax, expected a list but got something else.", declItem);
          break;
        }
        
        if (matches(declItem.getCar(), "type"))
        {          
          // Just skip past TYPE and process the type declarations
          declItem = (Pair) declItem.getCdr();
                  
          if (! (declItem.getCdr() instanceof Pair)) {
            Object save = pushPositionOf(declItem);
            error('e', "A type specifier must be applied to at least one declaration.");
            popPositionOf(save);
            break;
          }
          
          vars = (Pair) declItem.getCdr();
        }
        // .. other checks could be performed here
        else
        {
          // By default we process type declarations
          vars = (Pair) declItem.getCdr();
        }

        // e.g vars = (x y) or (z) or (k)
        // For each aliased variable, place it in the new lexical environment
        while (vars != LList.Empty)
        {
          if (! (vars instanceof Pair)) {
            Object save = pushPositionOf(vars);
            error('e', "The variable list in a declare form must be a proper list.");
            popPositionOf(save);
            break;
          }
          
          Object var = ((Pair) vars).getCar();
          Declaration varDecl = lexical.get(var);
          if (varDecl != null)
          {
            Declaration aliasedDecl = new Declaration(varDecl.getSymbol());
            ReferenceExp ref = new ReferenceExp(varDecl);
            letVariable(aliasedDecl, ref);
            aliasedDecl.setType(this.exp2Type(declItem));
            aliasedDecl.setFlag(Declaration.TYPE_SPECIFIED);
            aliasedDecl.setFlag(Declaration.IS_SINGLE_VALUE);
          }
          else
          {
            error('w', "No declaration seen for `" + var + "`");
          }
          vars = ((Pair) vars).getCdr();
        }
        decls = ((Pair) decls).getCdr();
      }
      letEnter();
      pushForm(letDone(super.rewrite_body(body)));
    } else
      super.rewriteBody(list);
  }
}
