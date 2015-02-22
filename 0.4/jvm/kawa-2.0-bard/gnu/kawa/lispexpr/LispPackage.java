package gnu.kawa.lispexpr;
import gnu.commonlisp.lang.CommonLisp;
import gnu.mapping.*;
import gnu.lists.*;
import java.util.Iterator;
import java.util.Stack;

/** Implement a Common Lisp "package" value.
 * 
 * @author Per Bothner
 * @author Charles Turner
 */

public class LispPackage extends Namespace
{
  /** The set of exported symbols.
   * This is one of the packages in importing.
   */
  public Namespace exported = new Namespace();
  
  public void setExportedNamespace (Namespace exp)
  {
    this.exported = exp;
  }
  
  /** The nicknames for this package. */
  LList nicknames = LList.Empty;
  
  private static final Object masterLock = new Object();

  LList shadowingSymbols = LList.Empty;
  
  public LList getShadowingSymbols()
  {
    return shadowingSymbols;
  }
  
  public static final LispPackage CLNamespace = 
      (LispPackage) valueOf("COMMON-LISP");
  public static final LispPackage KeywordNamespace =
      (LispPackage) valueOf("KEYWORD");
  public static final LispPackage KawaNamespace =
      (LispPackage) valueOf("KAWA");
  /* The class namespace is used to resolve Java class names in the context of
   types. The user would specific class::|java.lang.String| if she wanted to
   use java.lang.String as a type name. This avoids various ambiguities. */
  public static final LispPackage ClassNamespace = (LispPackage) valueOf("CLASS");

  /** Common Lisp {@code *package*} special. */
  public static ThreadLocation<LispPackage> currentPackage
      = new ThreadLocation<LispPackage>("*package*");
  
  static
  {
    nsTable.put("CL", CLNamespace);
    CLNamespace.nicknames = Pair.make("CL", CLNamespace.nicknames);
    /* Package CL inherits from this package to facilitate cross language
     * lookups */
    KawaNamespace.setExportedNamespace(EmptyNamespace);
    LispPackage.use(CLNamespace, KawaNamespace); // FIXME: Should be used from CL-USER
    LispPackage.use(CLNamespace, ClassNamespace);
    currentPackage.setGlobal(CLNamespace);
  }

  /** Namespaces that this Namespace imports or uses.
   * These are the <code>imported</code> fields of the
   * <code>NamespaceUse</code>, chained using <code>nextImported</code> fields.
   * The CommonLisp "uses" list. */
  NamespaceUse imported;
  /** Namespaces that import/use this namespace.
   * The CommonLisp "used-by" list. */
  NamespaceUse importing;
  
  /** Used for the CL PACKAGE-USE-LIST function. */
  public static LList pkgUsesList(LispPackage lp)
  {
    LList uses = LList.Empty;
    NamespaceUse it = lp.imported;
    while (it != null) {
      uses = Pair.make(it.imported, uses);
      it = it.nextImported;
    }
    return uses;
  }
  
  /** Used for the CL PACKAGE-USED-BY-LIST function */
  public static LList pkgUsedByList(LispPackage lp)
  {
    LList usedby = LList.Empty;
    NamespaceUse it = lp.importing;
    while (it != null) {
      usedby = Pair.make(it.importing, usedby);
      it = it.nextImporting;
    }
    return usedby;
  }
  
  public static void addNickNames(LispPackage name, LList nicks)
  {
    synchronized (nsTable)
    {
      for (Object nick : nicks) {
        name.nicknames = Pair.make((String) nick, name.nicknames);
        nsTable.put((String) nick, name);
      }
    }
  }
  
  public static void usePackages (LList importees, LispPackage importer)
  {
    for (Object usePkg : importees)
    {
      LispPackage lp;

      if (usePkg instanceof Symbol)
        lp = (LispPackage) LispPackage.valueOfNoCreate(((Symbol) usePkg).getName());//uc
      else if (usePkg instanceof LispPackage)
        lp = (LispPackage) usePkg;
      else
        lp = (LispPackage) LispPackage.valueOfNoCreate((String) usePkg);

      if (lp != null)
      {
        use(importer, lp);
      }
      else
      {
        throw new RuntimeException("The name " + usePkg + " does not designate any package");
     }
   }
 }
  
  public static LispPackage makeLispPackage (Object name, LList nicks,
                                             LList used)
  {
    LispPackage newpack = (LispPackage) LispPackage.valueOf((String) name);
    addNickNames(newpack, nicks);
    usePackages(used, newpack);
    return newpack;    
  }
  
  /** Look up a given package in the {@link Namespace} map.
   * 
   * This method creates a new Lisp package in the namespace if it does not
   * already exist.
   * 
   * @param name The name of the package to look up.
   * 
   * @return The {@link LispPackage} named by {@code name} or {@code null} if a
   *   {@link Namespace} is already named by {@code name} but is not a lisp
   *   package.
   */
  public static LispPackage valueOf (String name)
  {
    if (name == null)
      name = "";
    
    synchronized (nsTable) {
      Namespace ns = (Namespace) nsTable.get(name);
      if (ns != null)
        return (LispPackage) ns;
      ns = new LispPackage();
      ns.setName(name.intern());
      Namespace.nsTable.put(name, ns);
      return (LispPackage) ns;
    }
  }
  
  public static Namespace valueOfNoCreate (String name)
  {
    return (LispPackage) Namespace.valueOfNoCreate(name);
  }
  
  public Values.Values2 findSymbol (Object name)
  {
    String sname = name.toString();

    Symbol sym = exported.lookup(sname);
    if (sym != null)
    {
      return Values.values2(sym, CommonLisp.externalKeyword);
    }
    
    sym = lookupInternal(sname, sname.hashCode());
    if (sym != null)
    {
      return Values.values2(sym, CommonLisp.internalKeyword);
    }
    
    // It's not an exported or an imported symbol, let's check the inheritance
    // chain.
    NamespaceUse U = imported;
    while (U != null)
    {
      if (U.imported == LispPackage.KawaNamespace)
        sym = U.imported.exported.lookup(sname.toLowerCase());
      else
        sym = U.imported.exported.lookup(sname);

      if (sym != null)
      {
        return Values.values2(sym, CommonLisp.inheritedKeyword);
      }

      U = U.nextImported;
    }

    return Values.values2(CommonLisp.FALSE, CommonLisp.FALSE);
  }
  
  /** Export a list of symbols from a package, checking for conflicts.
   * 
   * @param syms The list of symbols to export.
   * @param pkg The package to export the symbols from.
   * 
   */
  public static void exportPkg (LList syms, LispPackage pkg)
  {
    Stack<Symbol> validSyms = new Stack<Symbol>();
    Iterator symiter = syms.getIterator();
    Symbol s;
    Values v;

    while (symiter.hasNext())
    {
      s = (Symbol) symiter.next();
      v = pkg.findSymbol(s.getName());//uc
      if (v.get(1) != CommonLisp.FALSE
          && !validSyms.contains(s))
      {
        validSyms.push(s);
      }
    }

    NamespaceUse usedBy = pkg.imported;
    symiter = syms.getIterator();

    while (symiter.hasNext())
    {
      s = (Symbol) symiter.next();
      String sname = s.getName();//uc
      while (usedBy != null)
      {
        v = usedBy.imported.findSymbol(sname);
        if (v.get(1) != CommonLisp.FALSE
            && v.get(0) != s
            && !usedBy.imported.shadowingSymbols.contains(v.get(0)))
        {
          // name conflict in usedBy.imported! Correctable, ask user
          // which name to nuke.
          signal("Name conflict from package " + usedBy.imported + "on symbol"
              + s);
        }
        usedBy = usedBy.nextImported;
      }
    }

    // Check that all symbols are accessible. If not, ask to import them.
    Stack<Symbol> missing = new Stack<Symbol>();
    // syms accessible in the inheritance chain, but not in this package
    Stack<Symbol> imports = new Stack<Symbol>();

    symiter = syms.getIterator();
    while (symiter.hasNext())
    {
      s = (Symbol) symiter.next();
      v = pkg.findSymbol(s.getName());//uc
      if ((v.get(1) == CommonLisp.FALSE)
          && (!(v.get(0).hashCode() == s.hashCode())))
      {
        missing.push(s);
      }
      else if (v.get(1) == KeywordNamespace.valueOf("inherited"))
      {
        imports.push(s);
      }
    }

    if (!missing.isEmpty())
    {
      // correctable error, ask user if they want ot import these
      // missing symbols into the package
      signal("The following symbols are missing: " + missing.toString());
    }

    while (!imports.isEmpty())
    {
      Symbol sym = imports.pop();
      pkg.exported.add(sym, sym.hashCode());
    }

    while (!validSyms.isEmpty())
    {
      s = validSyms.pop();
      pkg.remove(s); // remove internal
      pkg.exported.add(s, s.hashCode()); // add to external
    }
  }
  
  /**
   * Import a list of symbols into the internal table of a package.
   *
   * This method checks for conflicts, and should in the future allow the user
   * to shadow import any conflicts.
   *
   * @param syms the list of symbols to import.
   * @param pkg the package to import into.
   */
  public static void importPkg (LList syms, LispPackage pkg)
  {
    Stack<Symbol> validSyms = new Stack<Symbol>();
    Iterator symiter = syms.getIterator();
    Symbol s;
    Values v;

    while (symiter.hasNext())
    {
      s = (Symbol) symiter.next();
      v = pkg.findSymbol(s.getName());

      if (v.get(1) == CommonLisp.FALSE)
      {
        Iterator symiter2 = syms.getIterator();
        boolean found = false;
        while (symiter2.hasNext())
        {
          Symbol s2 = (Symbol) symiter2.next();
          if (s.getName().equals(s2.getName()))
          {
            if (s != s2)
            {
              validSyms.remove(s2);
              // name conflict
              signal("Symbol " + s2 + " conflicts with this package.");
            }
          }
        }

        if (!found)
        {
          validSyms.push(s);
        }
      }
      else if (v.get(0) != s)
      {
        // name conflict
        signal("Symbol " + v.get(0) + " conflicts in this package");
      }
      else if (v.get(1) == KeywordNamespace.valueOf("inherited"))
      {
        validSyms.add(s);
      }
    }

    while (!validSyms.isEmpty())
    {
      Symbol sym = validSyms.pop();
      pkg.add(sym, sym.hashCode());
    }
    // make any uninterned symbols owned by PKG
    symiter = syms.getIterator();
    while (symiter.hasNext())
    {
      s = (Symbol) symiter.next();
      if (s.getNamespace() == null)
      {
        s.setNamespace(pkg);
      }
    }
  }
  
  /**
   * The list of symbols managed by a given namespace.
   *
   * @param ns The namespace whose symbol table we query
   * @return The list of symbols managed by the given namespace.
   */
  public LList allSymbols (Namespace ns)
  {
    LList res = LList.Empty;
    java.util.Iterator symNameIter = ns.entrySet().iterator();
    while (symNameIter.hasNext())
    {
      res = Pair.make(symNameIter.next(), res);
    }
    return res;
  }
 
  /**
   * The list of symbols exported from this package.
   *
   * @return A list of the the exported symbols from the current package.
   *
   */
  public LList allExternalSymbols ()
  {
    return allSymbols(this.exported);
  }

  /**
   * The list of symbols interned into this package.
   *
   * @return A list of the interned symbols in this package.
   */
  public LList allInternalSymbols ()
  {
    return allSymbols(this);
  }

  public static void use (LispPackage importing, LispPackage imported)
  {
    synchronized (masterLock)
      {
	// FIXME check conflicts.
	NamespaceUse use = new NamespaceUse();
	use.nextImporting = imported.importing;
        use.importing = importing;
	imported.importing = use;
	use.nextImported = importing.imported;
        use.imported = imported;
	importing.imported = use;
      }
  }

  @Override
  public Symbol lookup(String name, int hash, boolean create)
  {
    Symbol sym = exported.lookup(name, hash, false);
    if (sym != null)
      return sym;
    sym = lookupInternal(name, hash);
    if (sym != null)
      return sym;

    // Do we need to synchronize on masterLock as well?  FIXME
    for (NamespaceUse used = imported;  used != null;
	 used = used.nextImported)
      {
	sym = lookup(name, hash, false);
	if (sym != null)
	  return sym;
      }

    if (create)
      return add(Symbol.makeUninterned(name, this), hash); // Optimization
    else
      return null;
  }

  public Symbol lookupPresent (String name, int hash, boolean intern)
  {
    Symbol sym = exported.lookup(name, hash, false);
    if (sym == null)
      sym = super.lookup(name, hash, intern);
    return sym;
  }

  public boolean isPresent (String name)
  {
    return lookupPresent(name, name.hashCode(), false) != null;
  }

  public boolean unintern (Symbol symbol)
  {
    String name = symbol.getName();
    int hash = name.hashCode();
    if (exported.lookup(name, hash, false) == symbol)
      exported.remove(symbol);
    else if (super.lookup(name, hash, false) == symbol)
      super.remove(symbol);
    else
      return false;
    symbol.setNamespace(null);
    if (removeFromShadowingSymbols(symbol))
      {
	// FIXME check use list:  If thee are two or more different symbols
	// named 'name' in used packages, then signal a conflict.
      }
    return true;
  }

  private void addToShadowingSymbols (Symbol sym)
  {
    for (Object s = shadowingSymbols;  s != LList.Empty; )
      {
	Pair p = (Pair) s;
	if (p.getCar() == sym)
	  return;
	s = p.getCdr();
      }
    shadowingSymbols = new Pair(sym, shadowingSymbols);
  }

  private boolean removeFromShadowingSymbols (Symbol sym)
  {
    Pair prev = null;
    for (Object s = shadowingSymbols;  s != LList.Empty; )
      {
	Pair p = (Pair) s;
	s = p.getCdr();
	if (p.getCar() == sym)
	  {
	    if (prev == null)
	      shadowingSymbols = (LList) s;
	    else
	      prev.setCdr(s);
	    return true;
	  }
	prev = p;
      }
    return false;
  }

  /** The core of the Common Lisp shadow function. */
  public void shadow (String name)
  {
    Symbol sym = lookupPresent(name, name.hashCode(), true);
    addToShadowingSymbols(sym);
  }

  public void shadowingImport (Symbol symbol)
  {
    String name = symbol.getName();
    int hash = name.hashCode();
    Symbol old = lookupPresent(name, name.hashCode(), false);
    if (old != null && old != symbol)
      unintern(old);
    addToShadowingSymbols(symbol);
  }
  
  /**
   * Temporary stub until Kawa supports conditional restarts.
   */
  public static void signal (String msg)
  {
    throw new RuntimeException(msg);
  }

}

/**
 * This is used to implement two linked lists. For performance they're combined
 * into one object.
 */
class NamespaceUse
{
  LispPackage imported = new LispPackage();
  NamespaceUse nextImported;
  LispPackage importing = new LispPackage();
  NamespaceUse nextImporting;
}
