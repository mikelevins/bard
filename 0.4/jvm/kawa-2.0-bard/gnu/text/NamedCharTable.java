package gnu.text;
import gnu.kawa.util.AbstractHashTable;
import gnu.kawa.util.GeneralHashTable;
import gnu.kawa.util.HashNode;

/** A table to manage standard character names.
 * This is conceptually a {@code HashMap<String,String>}, except that
 * the value strings are allocated lazily when the key is first requested.
 *
 * (Not sure if this is worth a separate class, but the code is
 * simple, it should make startup faster, and save having to
 * intern over 2200 value strings.)
 */

public class NamedCharTable extends GeneralHashTable<String, String> {

    protected HashNode<String, String> makeEntry (String key, int hash, String value) {
        return new Entry(key, value, hash);
    }

    public boolean appendTo(String key, Appendable out) {
        Entry e = (Entry) getNode(key);
        if (e == null)
            return false;
        e.appendTo(out);
        return true;
    }

    public void put(String name, int char1) {
        put(name, (String) null);
        Entry e = (Entry) getNode(name);
        e.char1 = char1;
    }

    public void put(String name, int char1, int char2) {
        put(name, null);
        Entry e = (Entry) getNode(name);
        e.char1 = char1;
        e.char2 = char2;
    }

   static class Entry extends HashNode<String, String> {
        int char1, char2;

        public Entry(String key, String value, int hash) {
            super(key, value, hash);
        }

        public void appendTo(Appendable out) {
            Char.print(char1, out);
            if (char2 != 0)
                Char.print(char2, out);
        }

        public synchronized String getValue() {
            String v = super.getValue();
            if (v == null) {
                StringBuilder sb = new StringBuilder(char2 == 0 ? 1 : 2);
                appendTo(sb);
                v = sb.toString();
                super.setValue(v);
            }
            return v;
        }
    }
}
