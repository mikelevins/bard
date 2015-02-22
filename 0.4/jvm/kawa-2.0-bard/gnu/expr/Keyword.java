package gnu.expr;

import gnu.lists.Consumer;
import gnu.mapping.*;
import gnu.text.Printable;
import java.io.*;

/**
 * Implementation of Lisp keywords.
 *
 * Keywords are symbolic identifiers that evaluate to themselves, as opposed to
 * symbols which generally name other values.
 *
 * @author Per Bothner
 */
public class Keyword extends Symbol implements Printable, Externalizable {
    public static final Namespace keywordNamespace = Namespace.create();

    static {
        keywordNamespace.setName("(keywords)");
    }

    public Keyword() {
    }

    private Keyword(String name) {
        super(name, keywordNamespace);
    }

    /**
     * Used for constructing literals (int gnu.expr.LitTable).
     */
    public Keyword(Namespace namespace, String name) {
        super(name, namespace);
    }

    /**
     * Get the corresponding non-keyword symbol.
     *
     * Informally, the symbol corresponding to dropping the ':'.
     */
    public Symbol asSymbol() {
        return Namespace.EmptyNamespace.getSymbol(getName());
    }

    /**
     * Create or find a Keyword with a given name (without ':').
     *
     * @param name the print-name of the desired Keyword
     * @return a Keyword with the given name, newly created iff none such exist
     */
    static public Keyword make(String name) {
        int hash = name.hashCode();
        Keyword keyword = (Keyword) keywordNamespace.lookup(name, hash, false);
        if (keyword == null) {
            keyword = new Keyword(name);
            keywordNamespace.add(keyword, hash);
        }
        return keyword;
    }

    /*
     public FString toSchemeString()
     {
     return new FString(name);
     }
     */
    /**
     * Test whether an object is a keyword.
     *
     * @param obj the object to test.
     * @return {@code true} if the object is a keyword, {@code false} otherwise.
     */
    public static boolean isKeyword(Object obj) {
        return obj instanceof Keyword;
    }

    @Override
    public final String toString() {
        return getName() + ':';
    }

    public void print(Consumer out) {
        Symbols.print(getName(), out);
        out.write(':');

    }

    /**
     * Search vals[0:offset-1] for a keyword.
     * Each key at vals[i] is followed by a value at keys[i+1].
     * (This is used to search for a keyword parameter in an argument list.)
     *
     * @param vals the list to search in
     * @param offset the index in vals to start the search at
     * @param keyword the keyword to search for
     * @return vals[i+1] such that vals[i]==keyword (and (i-offset) is even and
     * non-negative); if there is no such i, return Special.dfault.
     */
    public static Object searchForKeyword(Object[] vals,
                                          int offset, Object keyword) {
        for (int i = offset; i < vals.length; i += 2) {
            if (vals[i] == keyword) {
                return vals[i + 1];
            }
        }
        return Special.dfault;
    }

    /**
     * Search vals[0:offset-1] for a keyword.
     * Each key at vals[i] is followed by a value at keys[i+1].
     * (This is used to search for a keyword parameter in an argument list.)
     *
     * @param vals the list to search in
     * @param offset the index in vals to start the search at
     * @param keyword the keyword to search for
     * @param dfault the value to return if there is no match
     * @return vals[i+1] such that vals[i]==keyword (and (i-offset) is even and
     * non-negative); if there is no such i, return dfault.
     */
    public static Object searchForKeyword(Object[] vals,
                                          int offset, Object keyword,
                                          Object dfault) {
        for (int i = offset; i < vals.length; i += 2) {
            if (vals[i] == keyword) {
                return vals[i + 1];
            }
        }
        return dfault;
    }

    @Override
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(getName());
    }

    @Override
    public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
        name = (String) in.readObject();
    }

    @Override
    public Object readResolve() throws ObjectStreamException {
        return make(keywordNamespace, getName());
    }
}
