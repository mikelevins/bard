// Copyright (c) 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import java.util.*;
import gnu.mapping.*;
import gnu.kawa.util.GeneralHashTable;
import gnu.kawa.util.HashNode;

/** Manages the set of declarations "currently" in scope. */

public class NameLookup extends GeneralHashTable<Object,Declaration>
{
  Language language;

  public Language getLanguage () { return language; }
  public void setLanguage (Language language) { this.language = language; }

  public NameLookup (Language language)
  {
    this.language = language;
  }

  static final Symbol KEY = Symbol.makeUninterned("<current-NameLookup>");

  /** Get or create a NameLookup instance for a given Environment.
   * We want the same NameLookup instance to be used for multiple
   * interactive commands in the same "session", to preserve top-level
   * declarations.  We do that by registering it in the Environment.
   */
  public static NameLookup getInstance (Environment env, Language language)
  {
    Location loc = env.getLocation(KEY);
    NameLookup nl = (NameLookup) loc.get(null);
    if (nl == null)
      {
        nl = new NameLookup(language);
        loc.set(nl);
      }
    else
      nl.setLanguage(language);
    return nl;
  }

  public static void setInstance (Environment env, NameLookup instance)
  {
    if (instance == null)
      env.remove(KEY);
    else
      env.put(KEY, null, instance);
  }

  /** When true, top-level defs should push rather then replace old ones.
   * Otherwise, a module-level declaration should replace a matching
   * previous declaration, to avoid leaks.
   * However, sometimes we temporarily push new declarations that should not
   * permanently replace old declarations - for example when a hygienic
   * macro changes the current scope.
   */
  public boolean doSaveTopLevelRedefs() { return saveToplevelsRedefsCount > 0; }
  public void pushSaveTopLevelRedefs() { saveToplevelsRedefsCount++; }
  public void popSaveTopLevelRedefs() { saveToplevelsRedefsCount--; }
  int saveToplevelsRedefsCount = 0;

  public void push (Declaration decl)
  {
    Object symbol = decl.getSymbol();
    if (symbol == null)
      return;
    if (++num_bindings >= table.length)
      rehash();
    int hash = hash(symbol);
    HashNode<Object,Declaration> node = makeEntry(symbol, hash, decl);
    int index = hashToIndex(hash);
    if (decl.getContext() instanceof ModuleExp
        && ! doSaveTopLevelRedefs())
      {
        int dnamespace = language.getNamespaceOf(decl);
        HashNode<Object,Declaration> prevNode = null;
        HashNode<Object,Declaration> oldNode = table[index];
        while (oldNode != null)
          {
            Declaration oldDecl = oldNode.getValue();
            HashNode<Object,Declaration> oldNext = oldNode.next;
            if (oldDecl != null
                && decl.getSymbol() == oldDecl.getSymbol()
                && oldDecl.getContext() instanceof ModuleExp
                && decl.getContext() != oldDecl.getContext()
                && dnamespace == language.getNamespaceOf(oldDecl))
              {
                if (prevNode == null)
                  table[index] = oldNext;
                else
                  prevNode.next = oldNext;
                --num_bindings;
                break;
              }
            prevNode = oldNode;
            oldNode = oldNext;
          }
      }
    node.next = table[index];
    table[index] = node;
  }

  public boolean pop (Declaration decl)
  {
    Object symbol = decl.getSymbol();
    if (symbol == null)
      return false;
    int hash = hash(symbol);
    HashNode prev = null;
    int index = hashToIndex(hash);
    HashNode node = table[index];
    while (node != null)
      {
	HashNode next = node.next;
        if (node.getValue() == decl)
	  {
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.next = next;
	    num_bindings--;
	    return true;
	  }
	prev = node;
	node = next;
      }
    return false;
  }

  public void push (ScopeExp exp)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      push(decl);
  }

  public void pop (ScopeExp exp)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      pop(decl);
  }

  /** Remove visible declarations subsumed (hidden) by a given declaration.
   * This is primarily used to avoid memory leaks.
   */
  public void removeSubsumed (Declaration decl)
  {
    Object symbol = decl.getSymbol();
    int hash = hash(symbol);
    int index = hashToIndex(hash);
    HashNode prev = null;
    for (HashNode node = table[index]; node != null;  )
      {
        HashNode next = node.next;
        Declaration ndecl = (Declaration) node.getValue();
        if (ndecl != decl && subsumedBy(decl, ndecl))
          {
            if (prev == null)
              table[index] = next;
            else
              prev.next = next;
          }
        else
          prev = node;
        node = next;
      }
  }

  /** True if decl subsumes (hides) other. */
  protected boolean subsumedBy(Declaration decl, Declaration other)
  {
    return decl.getSymbol() == other.getSymbol()
      && (language.getNamespaceOf(decl) & language.getNamespaceOf(other)) != 0;
  }

  public Declaration lookup (Object symbol, int namespace)
  {
    int hash = hash(symbol);
    int index = hashToIndex(hash);
    for (HashNode node = table[index];
	 node != null;  node = node.next)
      {
        Declaration decl = (Declaration) node.getValue();
	if (symbol.equals(decl.getSymbol())
	    && language.hasNamespace(decl, namespace))
	  return decl;
      }
    return null;
  }

  public Declaration lookup (Object symbol, boolean function)
  {
    return lookup(symbol, (function ? Language.FUNCTION_NAMESPACE
			   : Language.VALUE_NAMESPACE));
  }
}
