// Copyright (c) 2009, 2013 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.lang;
import gnu.expr.*;
import gnu.mapping.Symbol;
import gnu.lists.*;
import gnu.text.SourceLocator;
import java.io.*;

/**
 * Helper method and implementation classes for SyntaxForm.
 * @author Per Bothner
 */
public class SyntaxForms {

    public static Object makeForm (Object datum, TemplateScope scope) {
        if (datum instanceof SyntaxForm || datum == LList.Empty
            // Self-evaluating values don't need to be wrapped
            // This is an approximation - can be tweaked later.
            || datum instanceof Number || datum instanceof Keyword
            || scope == null)
            return datum;
        if (datum instanceof PairWithPosition)
            return new PairWithPositionSyntaxForm((PairWithPosition) datum,
                                                  scope);
        if (datum instanceof Pair)
            return new PairSyntaxForm((Pair) datum, scope);
        return new SimpleSyntaxForm(datum, scope);
    }

    /** Create a syntax object with specified datum, and given syntatic context.
     * Used to implement {@code datum->syntax}.
     * @param template If this is a {@code SyntaxForm}, use its scope;
     *   otherwise use the current {@code Compilation}'s current scope.
     *   (This means just returning the datum as-is.)
     * @param datum The value (S-expression datum) to use.
     * @param srcloc Used to set source location (line number etc).
     *   Ignored if null; otherwise should be a {@code SourceLocator}.
     */
    public static Object makeWithTemplate(Object template, Object datum,
                                          Object srcloc) {
        if (srcloc instanceof SourceLocator
            && datum instanceof Pair) {
            Pair pdatum = (Pair) datum;
            SourceLocator sloc = (SourceLocator) srcloc;
            if (template instanceof SyntaxForm)
                return new PairWithPositionSyntaxForm(pdatum, sloc,
                                                      ((SyntaxForm) template).getScope());
            else
                return new PairWithPosition(sloc, pdatum.getCar(), pdatum.getCdr());
        }
        if (datum instanceof SyntaxForm)
            return (SyntaxForm) datum;
        if (template instanceof SyntaxForm) {
            SyntaxForm sdatum = (SyntaxForm) template;
            if (datum == sdatum.getDatum())
                return sdatum;
            return fromDatum(datum, sdatum);
        }
        return datum;
    }

    public static Object makeWithTemplate(Object template, Object form) {
        return makeWithTemplate(template, form, null);
    }

    /** Utility method to implement Scheme free-identifier=? and bound-identifier=?.
     * @param id1 An identifier - either a symbol or a SyntaxForm whose form is a symbol.  We assume it satisfies the Scheme predicate identifier?.
     * @param id2 The other identifier to compare against.
     * @param checkBound true for bound-identifier=? and false for free-identifier=?.
     */
    public static boolean identifierEquals (Object id1, Object id2, boolean checkBound) {
        Compilation comp = (Translator) Compilation.getCurrent();
        Object s1, s2;
        TemplateScope sc1, sc2;
        if (id1 instanceof SyntaxForm) {
            SyntaxForm sf = (SyntaxForm) id1;
            s1 = sf.getDatum();
            sc1 = sf.getScope();
        }
        else {
            s1 = id1;
            sc1 = null;
        }
        if (id2 instanceof SyntaxForm) {
            SyntaxForm sf = (SyntaxForm) id2;
            s2 = sf.getDatum();
            sc2 = sf.getScope();
        }
        else {
            s2 = id2;
            sc2 = null;
        }
        if (s1 != s2)
            return false;
        if (sc1 == sc2)
            return true;
        if (checkBound) {
            // Note that SRFI-72 specifies:
            // (bound-identifier=? (syntax x) (syntax x)) => #f
            // but MzScheme/Racket and Chez Scheme return #t.
            // SRFI-72 says: "Two identifiers will also be bound-identifier=?
            // if they were produced from existing bound-identifier=?
            // identifiers during a single evaluation of the same syntax or
            // quasisyntax form ..."
            // but R6RS specifies bound-identifier=? in terms of "marks"
            // which are applied when a transformer is applied, thus two
            // syntax forms in the same transformer have the same marks.
            Object mark1 = sc1 != null ? sc1.macroMark : null;
            Object mark2 = sc2 != null ? sc2.macroMark : null;
            return mark1 == mark2;
        }
        ScopeExp savedScope = comp.currentScope();
        if (sc1 != null)
            comp.setCurrentScope(sc1);
        Declaration d1 = comp.lexical.lookup(s1, -1);
        comp.setCurrentScope(sc2 != null ? sc2 : savedScope);
        Declaration d2 = comp.lexical.lookup(s2, -1);
        if (sc2 != null)
            comp.setCurrentScope(savedScope);
        return d1 == d2;
    }

  public static boolean isIdentifier (SyntaxForm form)
  {
    return form.getDatum() instanceof Symbol;
  }

 /** Make a SyntaxForm object with the same contextual information as this.
   * @param datum which used for the new syntax value.
   * Corresponds to the <code>datum-&gt;syntax-object</code> function.
   */
  public static Object fromDatum (Object datum, SyntaxForm template)
  {
    return SyntaxForms.makeForm(datum, template.getScope());
  }

    public static Object fromDatumIfNeeded(Object datum, SyntaxForm template) {
        if (datum instanceof SyntaxForm || template == null)
            return datum;
        else if (datum == template.getDatum())
            return template;
        else
            return SyntaxForms.fromDatum(datum, template);
    }

  public static Expression rewrite (Object x)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    return tr.rewrite(x);
  }

  public static Expression rewriteBody (Object x)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    return tr.rewrite_body(x);
  }

  public static final boolean DEBUGGING = true;

  public static String toString (SyntaxForm sform, String id)
  {
    StringBuilder sbuf = new StringBuilder("#<syntax");
    if (DEBUGGING && id != null)
      {
        sbuf.append('#');
        sbuf.append(id);
      }
    sbuf.append(' ');
    sbuf.append(sform.getDatum());
    if (DEBUGGING)
      {
        TemplateScope scope = sform.getScope();
        if (scope == null)
          {
            sbuf.append(" in null");
          }
        else
          {
            sbuf.append(" in #");
            sbuf.append(scope.id);
          }
      }
    sbuf.append(">");
    return sbuf.toString();
  }

    public static class SimpleSyntaxForm implements SyntaxForm, Externalizable {
        private Object datum;
        private TemplateScope scope;

        // DEBUGGING:
        static int counter;
        int id = ++counter;

        public SimpleSyntaxForm(Object datum, TemplateScope scope) {
            this.datum = datum;
            this.scope = scope;
        }

        public Object getDatum() {
            return datum;
        }

        public TemplateScope getScope() {
            return scope;
        }

        public String toString() {
            String sid = DEBUGGING ? Integer.toString(id) : null;
            return SyntaxForms.toString(this, sid);
        }

        public void writeExternal(ObjectOutput out) throws IOException {
            out.writeObject(datum);
            out.writeObject(scope);
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            datum = in.readObject();
            scope = (TemplateScope) in.readObject();
        }
    }

    public static class PairSyntaxForm extends ImmutablePair
        implements SyntaxForm, Externalizable {

        private Pair datum;
        private TemplateScope scope;

        public PairSyntaxForm(Pair datum, TemplateScope scope) {
            this.datum = datum;
            this.scope = scope;
        }

        public Object getDatum() {
            return datum;
        }

        public TemplateScope getScope() {
            return scope;
        }

        public Object getCar() {
            if (car == null)
                car = SyntaxForms.makeForm(datum.getCar(), scope);
            return car;
        }

        public Object getCdr() {
            if (cdr == null)
                cdr = SyntaxForms.makeForm(datum.getCdr(), scope);
            return cdr;
        }

        public String toString() {
            //String sid = DEBUGGING ? Integer.toString(id) : null;
            return SyntaxForms.toString(this, null);
        }

        public void writeExternal(ObjectOutput out) throws IOException {
            out.writeObject(datum);
            out.writeObject(scope);
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            datum = (Pair) in.readObject();
            scope = (TemplateScope) in.readObject();
        }
    }

    public static class PairWithPositionSyntaxForm extends PairWithPosition
        implements SyntaxForm, SourceLocator, Externalizable {
        private PairWithPosition datum;
        private TemplateScope scope;

        public PairWithPositionSyntaxForm(PairWithPosition datum,
                                          TemplateScope scope) {
            // The inherited car and cdr fields are initialized to null
            // because they're used as caches -
            // see the getCar and getCdr methods below
            super(datum, null, null);
            this.datum = datum;
            this.scope = scope;
        }

        public PairWithPositionSyntaxForm(Pair datum,
                                          SourceLocator where,
                                          TemplateScope scope) {
            this(new PairWithPosition(where, datum.getCar(), datum.getCdr()),
                 scope);
        }

        public Object getDatum() {
            return datum;
        }

        public TemplateScope getScope() {
            return scope;
        }

        public Object getCar () {
            if (car == null)
                car = SyntaxForms.makeForm(datum.getCar(), scope);
            return car;
        }

        public Object getCdr () {
            if (cdr == null)
                cdr = SyntaxForms.makeForm(datum.getCdr(), scope);
            return cdr;
        }
        public String toString () {
            //String sid = DEBUGGING ? Integer.toString(id) : null;
            return SyntaxForms.toString(this, null);
        }

        public void writeExternal(ObjectOutput out) throws IOException {
            out.writeObject(datum);
            out.writeObject(scope);
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            datum = (PairWithPosition) in.readObject();
            scope = (TemplateScope) in.readObject();
        }
    }

  // TODO: static class VectorSyntaxForm ...
}
