package kawa.lang;
import java.util.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.reflect.Invoke;
import gnu.bytecode.ClassType;
import gnu.bytecode.Method;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.kawa.functions.CompileNamedPart;
import gnu.kawa.functions.MakeSplice;

/**
 * The Syntax transformer that re-writes the "quote" "quasiquote" primitive.
 * In both cases recursively resolves SyntaxForm wrappers and resolves
 * namespaces of symbols.  In the case of quasiquote also handles unquoting.
 * @author	Per Bothner
 */

public class Quote extends Syntax {
    public static final Quote plainQuote = new Quote("quote", false);
    public static final Quote quasiQuote = new Quote("quasiquote", true);

    public Quote (String name, boolean isQuasi) {
        super(name);
        this.isQuasi = isQuasi;
    }

    protected boolean matchesUnquote(Pair pair, SyntaxForm syntax,
                                     Translator tr) {
        return tr.matches(pair.getCar(), syntax, LispLanguage.unquote_str);
    }

    protected boolean matchesUnquoteSplicing(Pair pair, SyntaxForm syntax,
                                     Translator tr) {
        return tr.matches(pair.getCar(), syntax, LispLanguage.unquotesplicing_str);
    }

    protected boolean matchesQuasiQuote(Object form, SyntaxForm syntax,
                                     Translator tr) {
        return tr.matches(form, syntax, LispLanguage.quasiquote_str);
    }

    /** An initial value for 'depth' for plain (non-quasi) quote. */
    protected static final int QUOTE_DEPTH = -1;

    /** True for quasiquote; false for plain quote. */
    protected boolean isQuasi;

    protected Object expand(Object template, int depth, Translator tr) {
        /* #ifdef use:java.util.IdentityHashMap */ 
        IdentityHashMap seen = new IdentityHashMap();
        /* #else */
        // Object seen = null;
        /* #endif */
        return expand(template, depth, null, seen, tr);
    }

    /** Quote an object (without namespace-expansion).
     * Basically just recursively removes SyntaxForm wrappers. */
    public static Object quote(Object obj, Translator tr) {
        return plainQuote.expand(obj, QUOTE_DEPTH, tr);
    }

    /** Quote an object (without namespace-expansion).
     * Basically just recursively removes SyntaxForm wrappers. */
    public static Object quote(Object obj) {
        return plainQuote.expand(obj, QUOTE_DEPTH, (Translator) Compilation.getCurrent());
    }

    protected Expression coerceExpression(Object val, Translator tr) {
        return val instanceof Expression ? (Expression) val : leaf(val, tr);
    }

    protected Expression leaf(Object val, Translator tr) {
        return new QuoteExp(val);
    }

    protected boolean expandColonForms() {
        return true;
    }

    public static Symbol makeSymbol(Namespace ns, Object local) {
        String name;
        /* #ifdef use:java.lang.CharSequence */
        if (local instanceof CharSequence)
            name = ((CharSequence) local).toString();
        /* #else */
        // if (local instanceof gnu.lists.CharSeq)
        //  name = ((gnu.lists.CharSeq) local).toString();
        /* #endif */
        else
            name = (String) local;
        return ns.getSymbol(name.intern());
    }

    Object expand_pair(Pair list, int depth, SyntaxForm syntax,
                       Object seen, Translator tr) {
        Pair pair = list;
        Object cdr;
        Object rest;
        for (;;) {
            // This would be simpler as plain recursion, but we try to iterate
            // over the given list, partly for speed, but more importantly
            // to avoid stack overflow in the case of long lists.
            rest = pair;
            Pair p1, p2;
            boolean isUnquote;
            // We're currently examining pair, which is the n'th cdr of list.
            // All previous elements (cars) are returned identically by expand.
            // What makes things complicated is that to the extent that no changes
            // are needed, we want to return the input list as-is.
            if (expandColonForms()
                && tr != null
                && pair == list
                && tr.matches(pair.getCar(), syntax, LispLanguage.lookup_sym)
                && pair.getCdr() instanceof Pair
                && (p1 = (Pair) pair.getCdr()) instanceof Pair
                && (p2 = (Pair) p1.getCdr()) instanceof Pair
                && p2.getCdr() == LList.Empty) {
                Expression part1 = tr.rewrite_car(p1, false);
                Expression part2 = tr.rewrite_car_for_lookup(p2);
                Namespace ns = tr.namespaceResolvePrefix(part1);
                Symbol sym = tr.namespaceResolve(ns, part2);
                if (sym != null)
                    cdr = sym;
                else if (ns != null && depth == 1)
                    cdr = new ApplyExp(quoteType.getDeclaredMethod("makeSymbol", 2),
                                       new Expression[] { QuoteExp.getInstance(ns),
                                                          part2 });
                else if (p1.getCar() instanceof SimpleSymbol
                         && part2 instanceof QuoteExp) {
                    String s1 = ((QuoteExp) part2).getValue().toString();
                    String s2 = p1.getCar().toString();
                    cdr = Symbol.makeWithUnknownNamespace(s1, s2);
                } else {
                    String combinedName
                        = CompileNamedPart.combineName(part1, part2);
                    if (combinedName != null)
                        cdr = tr.getGlobalEnvironment().getSymbol(combinedName);
                    else
                        cdr = pair;
                }
                break;
            } else if (depth < 0) {
            } else if (matchesQuasiQuote(pair.getCar(), syntax, tr))
                depth++;
            else if ((isUnquote = matchesUnquote(pair, syntax, tr))
                     || matchesUnquoteSplicing(pair, syntax, tr)) {
                depth--;
                Pair pair_cdr;
                if (! (pair.getCdr() instanceof Pair)
                    || (pair_cdr = (Pair) pair.getCdr()).getCdr() != LList.Empty
                    // Can't splice in cdr position (i.e. following dot).
                    || (depth == 0 && ! isUnquote))
                    return tr.syntaxError ("invalid used of " + pair.getCar() +
                                           " in quasiquote template");
                if (depth == 0) {
                    cdr = tr.rewrite_car(pair_cdr, syntax);
                    break;
                }
            }
 
            if (depth == 1 && pair.getCar() instanceof Pair) {
                Object form = pair.getCar();
                SyntaxForm subsyntax = syntax;
                while (form instanceof SyntaxForm) {
                    subsyntax = (SyntaxForm) form;
                    form = subsyntax.getDatum();
                }
                int splicing = -1;
                if (form instanceof Pair) {
                    Pair pform = (Pair) form;
                    if (matchesUnquote(pform, subsyntax, tr))
                        splicing = 0;
                    else if (matchesUnquoteSplicing(pform, subsyntax, tr))
                        splicing = 1;
                }
                if (splicing >= 0) {
                    form = ((Pair) form).getCdr(); // skip "unquote[splicing]".
                    Vector vec = new Vector();
                    cdr = null;
                    // R5RS allows only a single argument.  But
                    // see Bawden: Quasiquotation in Lisp (1999), Appendix B.
                    for (;;) {
                        if (form instanceof SyntaxForm) {
                            subsyntax = (SyntaxForm) form;
                            form = subsyntax.getDatum();
                        }
                        if (form == LList.Empty)
                            break;
                        if (form instanceof Pair) {
                            vec.addElement(tr.rewrite_car((Pair) form, subsyntax));
                            form = ((Pair) form).getCdr();
                        }
                        else
                            return tr.syntaxError("improper list argument to unquote");
                    }
                    int nargs = vec.size() + 1;
                    cdr = expand(pair.getCdr(), 1, syntax, seen, tr);
                    if (nargs > 1) {
                        Expression[] args = new Expression[nargs];
                        vec.copyInto(args);
                        args[nargs-1] = coerceExpression(cdr, tr);
                        Method method = splicing == 0 ? consXMethod : appendMethod;
                        cdr = new ApplyExp(method, args);
                    }
                    rest = pair;
                    break;
                }
            }
            Object car = expand (pair.getCar(), depth, syntax, seen, tr);
            if (car == pair.getCar()) {
                rest = pair.getCdr();
                if (rest instanceof Pair) {
                    IdentityHashMap map = (IdentityHashMap) seen;
                    Object old = map.get(rest);
                    if (old == null) {
                        map.put(rest, WORKING);
                        pair = (Pair) rest;
                        continue;
                    }
                }
                cdr = expand(rest, depth, syntax, seen, tr);
                if (cdr == rest)
                    return list;
                break;
            }
            cdr = expand (pair.getCdr(), depth, syntax, seen, tr);
            if (car instanceof Expression || cdr instanceof Expression) {
                Expression[] args = new Expression[2];
                args[0] = coerceExpression(car, tr);
                args[1] = coerceExpression(cdr, tr);
                cdr = new ApplyExp(makePairMethod, args);
            }
            else
                cdr = Translator.makePair(pair, car, cdr);
            break;
        }
        // rest is the n'th cdr of list.  cdr is the expansion of rest.
        // The first n cars of list are returned identically by expand.
        // These do need to be copied because cdr!=rest.
        if (list == rest)
            return cdr;
        Pair p = list;
        Pair[] pairs = new Pair[20];
        int npairs = 0;
        for (;;) {
            if (npairs >= pairs.length) {
                Pair[] tmp = new Pair[2 * npairs];
                System.arraycopy(pairs, 0, tmp, 0, npairs);
                pairs = tmp;
            }
            pairs[npairs++] = p;
            if (p.getCdr() == rest)
                break;
            p = (Pair) p.getCdr();
        }
        Object result = cdr instanceof Expression ? LList.Empty : cdr;
        while (--npairs >= 0) {
            p = pairs[npairs];
            result = Translator.makePair(p, p.getCar(), result);
        }

        if (cdr instanceof Expression) {
            Expression[] args = new Expression[2];
            args[1] = (Expression) cdr;
            if (npairs == 1) {
                // The n==1 case: Only a single pair before rest.
                args[0] = leaf(list.getCar(), tr);
                return new ApplyExp(makePairMethod, args);
            } else {
                args[0] = leaf(result, tr);
                return new ApplyExp(appendMethod, args);
            }
        }
        return result;
    }

    // Note in 'seen' map that datum is currently being expanded.
    private static final Object WORKING = new String("(working)");
    // Note in 'seen' map that datum is used multiple times, partly in cycle.
    private static final Object SHARED = new String("(shared)");

    /** Backquote-expand a template.
     * @param template the quasiquoted template to expand
     * @param depth - the (net) number of quasiquotes we are inside.
     *   The value QUOTE_DEPTH is a special case when we're inside
     *   a quote rather than a quasiquote.
     * @param tr the rewrite context
     * @return the expanded Expression (the result can be a non-expression,
     *   in which case it is implicitly a QuoteExp).
     */
    Object expand(Object template, int depth,
                  SyntaxForm syntax, Object seen, Translator tr) {
        /* #ifdef use:java.util.IdentityHashMap */ 
        IdentityHashMap map = (IdentityHashMap) seen;
        Object old = map.get(template);
        if (old == WORKING) {
            map.put(template, SHARED);
            return template;
        } else if (old == SHARED) {
            return template;
        } else if (old != null)
            return old;
        map.put(template, WORKING);
        /* #endif */
        Object result;
        if (template instanceof Pair)
            result = expand_pair ((Pair) template, depth, syntax, seen, tr);
        else if (template instanceof SyntaxForm) {
            syntax = (SyntaxForm) template;
            result = expand(syntax.getDatum(), depth, syntax, seen, tr);
        } else if (template instanceof FVector) {
            FVector vector = (FVector) template;
            int n = vector.size();
            Object[] buffer = new Object[n];
            // For each element, the state is one of these four:
            // 0: the expanded element is the same as the original
            // 1: the expanded element is a constant
            // 2: the expanded element is neither constant nor a splice
            // 3: the element is spliced in
            byte[] state = new byte[n];
            byte max_state = 0;
            for (int i = 0;  i < n; i++) {
                Object element = vector.get(i);
                int element_depth = depth;
                Pair pair;
                if (element instanceof Pair && depth > QUOTE_DEPTH
                    && matchesUnquoteSplicing((pair = (Pair)element), syntax, tr)
                    && --element_depth == 0) {
                    Pair pair_cdr;
                    if (! (pair.getCdr() instanceof Pair)
                        || (pair_cdr = (Pair) pair.getCdr()).getCdr() != LList.Empty)
                        return tr.syntaxError ("invalid used of " + pair.getCar() +
                                               " in quasiquote template");
                    buffer[i] = tr.rewrite_car(pair_cdr, syntax);
                    state[i] = 3;
                } else {
                    buffer [i] = expand (element, element_depth, syntax, seen, tr);
                    if (buffer[i] == element)
                        state[i] = 0;
                    else if (buffer[i] instanceof Expression)
                        state[i] = 2;
                    else
                        state[i] = 1;
                }
                if (state[i] > max_state)
                    max_state = state[i];
            }
            if (max_state == 0)
                result = vector;
            else if (max_state == 1)
                result = new ConstVector<Object>(buffer);
            else {
                Expression[] args = new Expression[n];
                int firstSpliceArg = -1;
                for (int i = 0;  i < n;  i++) {
                    if (state[i] == 3)
                        args[i] = new ApplyExp(MakeSplice.quoteInstance,
                                               (Expression) buffer[i]);
                    else
                        args[i] = coerceExpression (buffer[i], tr);
                }
                ApplyExp exp = makeInvokeMakeVector(args);
                exp.firstSpliceArg = firstSpliceArg;
                result = exp;
            }
        }
        else
            result = template;
        /* #ifdef use:java.util.IdentityHashMap */ 
        if (template != result && map.get(template) == SHARED)
            tr.error('e', "cycle in non-literal data");
        map.put(template, result);
        /* #endif */
        return result;
    }

    private static ApplyExp makeInvokeMakeVector(Expression[] args) {
        return new ApplyExp(makeVectorMethod, args);
    }

    public Expression rewrite(Object obj, Translator tr) {
        Pair pair;
        if (! (obj instanceof Pair)
            || (pair = (Pair) obj).getCdr() != LList.Empty)
            return tr.syntaxError ("wrong number of arguments to quote");
        return coerceExpression(expand(pair.getCar(), isQuasi ? 1 : QUOTE_DEPTH, tr), tr);
    }

    /** A wrapper around LList.consX to make it a "variable-arg method". */
    public static Object consX$V(Object[] args) {
        return LList.consX(args);
    }

    /** Same as regular append, but handle SyntaxForm wrappers. */
    public static Object append$V(Object[] args) {
        int count = args.length;
        if (count == 0)
            return LList.Empty;
        Object result = args[count - 1];
        for (int i = count - 1; --i >= 0; ) {
            Object list = args[i];
            Object copy = null;
            Pair last = null;
            SyntaxForm syntax = null;
            for (;;) {
                while (list instanceof SyntaxForm) {
                    syntax = (SyntaxForm) list;
                    list = syntax.getDatum();
                }
                if (list == LList.Empty)
                    break;
                Pair list_pair = (Pair) list;
                Object car = list_pair.getCar();
                if (syntax != null && ! (car instanceof SyntaxForm))
                    car = SyntaxForms.makeForm(car, syntax.getScope());
                Pair new_pair = new Pair(car, null);
                if (last == null)
                    copy = new_pair;
                else
                    last.setCdr(new_pair);
                last = new_pair;
                list = list_pair.getCdr();
            }
            if (last != null) {
                last.setCdr(result);
                result = copy;
            }
        }
        return result;
    }

    static final ClassType quoteType = ClassType.make("kawa.lang.Quote");
    static final Method consXMethod = quoteType.getDeclaredMethod("consX$V", 1);
    static final Method appendMethod = quoteType.getDeclaredMethod("append$V", 1);
    static final Method makePairMethod = Compilation.typePair.getDeclaredMethod("make", 2);
    static final Method makeVectorMethod = ClassType.make("gnu.lists.ConstVector")
        .getDeclaredMethod("make", 1);
}
