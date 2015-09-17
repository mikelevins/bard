package gnu.mapping;
/* #ifdef JAVA2 */
import java.lang.ref.WeakReference;
/* #endif */

/** Maps 2 objects to another.
 * Uses a weak references to each key, unless it is null or a Symbol.
 * This should at some point be merged with SimpleEnvironment.  FIXME.
 */

public class Table2D
{
  private static Table2D instance = new Table2D();
  public static final Table2D getInstance () { return instance; }

  Entry[] table;
  int log2Size;
  int mask;
  int num_bindings;

  public Table2D ()
  {
    this(64);
  }

  public Table2D (int capacity)
  {
    log2Size = 4;
    while (capacity > (1 << log2Size))
      log2Size++;
    capacity = 1 << log2Size;
    table = new Entry[capacity];
    mask = capacity - 1;
  }

  public Object get (Object key1, Object key2, Object defaultValue)
  {
    int h1 = System.identityHashCode(key1);
    int h2 = System.identityHashCode(key2);
    Entry entry = lookup(key1, key2, h1, h2, false);
    return entry == null || entry.value == entry ? defaultValue : entry.value;
  }

  public boolean isBound (Object key1, Object key2)
  {
    int h1 = System.identityHashCode(key1);
    int h2 = System.identityHashCode(key2);
    Entry entry = lookup(key1, key2, h1, h2, false);
    return entry != null && entry.value != entry;
  }

  public Object put (Object key1, Object key2, Object newValue)
  {
    int h1 = System.identityHashCode(key1);
    int h2 = System.identityHashCode(key2);
    Entry entry = lookup(key1, key2, h1, h2, true);
    Object oldValue = entry.getValue();
    entry.value = newValue;
    return oldValue;
  }

  public Object remove (Object key1, Object key2)
  {
    int h1 = System.identityHashCode(key1);
    int h2 = System.identityHashCode(key2);
    int hash = h1 ^ h2;
    return remove (key1, key2, hash);
  }

  public Object remove (Object key1, Object key2, int hash)
  {
    return remove (key1, key2, hash);
  }

  public Object remove (Object key1, Object key2, int hash1, int hash2)
  {
    int hash = hash1 ^ hash2;
    int index = hash & mask;
    Entry prev = null;
    Entry first = table[index];
    for (Entry e = first; e != null; )
      {
        Object k1 = e.key1;
        Object k2 = e.key2;
        boolean dead = false;
        /* #ifdef JAVA2 */
        if (k1 instanceof WeakReference)
          {
            k1 = ((WeakReference) k1).get();
            dead = k1 == null;
          }
        if (k2 instanceof WeakReference)
          {
            k2 = ((WeakReference) k2).get();
            dead = k2 == null;
          }
        /* #endif */
        Entry next = e.chain;
        Object oldValue = e.value;
        boolean matches = k1 == key1 && k2 == key2;
        if (dead || matches)
          {
            if (prev == null)
              table[index] = next;
            else
              prev.chain = next;
            num_bindings--;
            e.value = e;
          }
        else if (matches)
          return oldValue;
        else
          prev = e;
        e = next;
      }
    return null;

    /*
    // FIXME: clear reference queue
    
    Entry first = table[index];
    Entry prev = null;
    Entry e = first;
    for (;;)
      {
        if (e == null)
          return null;
        if (e.hash == hash && e.matches(key1, key2))
          break;
        prev = e;
        e = e.chain;
      }

    Object oldValue = e.value;
    e.value = e;

    // If this the head of a non-empty list, we can't unlink it.
    if ((key2 == null && e.next1 != e)
        || (key1 == null && e.next2 != e))
      return oldValue;

    Entry head1 = lookup(key1, null, hash1, 0, false);

    if (prev == null)
      table[index] = null;
    else
      prev.chain = e.chain;

    Entry head2 = lookup(key2, null, hash2, 0, false);
    for (Entry p = head1;  ;  p = p.next1)
      {
        Entry next = p.next;
        if (next1 == e)
          {
            p.next1 = e.next1;
            if (head1.next1 == head1 && head1.entry == head1)
              {
              }
            break;
          }
      }
    for (Entry e = table[index];  e != null;  e = e.chain)
      {
        //if (e.hash != hash)
      }
    return entry == null ? defaultValue : entry.getValue();
    */
  }

  void rehash ()
  {
    Entry[] oldTable = this.table;
    int oldCapacity = oldTable.length;
    int newCapacity = 2 * oldCapacity;
    Entry[] newTable = new Entry[newCapacity];
    int newMask = newCapacity - 1;
    num_bindings = 0;
    for (int i = oldCapacity;  --i >= 0;)
      {
        Entry first = oldTable[i];
        Entry prev = null;
	for (Entry e = first; e != null; )
	  {
            Object k1 = e.key1;
            Object k2 = e.key2;
            boolean dead = false;
            /* #ifdef JAVA2 */
            if (k1 instanceof WeakReference)
              {
                k1 = ((WeakReference) k1).get();
                dead = k1 == null;
              }
            if (k2 instanceof WeakReference)
              {
                k2 = ((WeakReference) k2).get();
                dead = k2 == null;
              }
            /* #endif */
            Entry next = e.chain;
            if (dead)
              e.value = e;
            else
              {
                int hash = System.identityHashCode(k1)
                  ^ System.identityHashCode(k2);
                int index = hash & newMask;
                e.chain = newTable[index];
                newTable[index] = e;
                num_bindings++;
              }
            e = next;
          }
      }
    table = newTable;
    log2Size++;
    mask = newMask;
  }

  protected Entry lookup (Object key1, Object key2, int hash1, int hash2,
                          boolean create)
  {
    int hash = hash1 ^ hash2;
    int index = hash & mask;
    Entry prev = null;
    Entry first = table[index];
    for (Entry e = first; e != null; )
      {
        Object k1 = e.key1;
        Object k2 = e.key2;
        boolean dead = false;
        /* #ifdef JAVA2 */
        if (k1 instanceof WeakReference)
          {
            k1 = ((WeakReference) k1).get();
            dead = k1 == null;
          }
        if (k2 instanceof WeakReference)
          {
            k2 = ((WeakReference) k2).get();
            dead = k2 == null;
              dead = true;
          }
        /* #endif */
        Entry next = e.chain;
        if (dead)
          {
            if (prev == null)
              table[index] = next;
            else
              prev.chain = next;
            num_bindings--;
            e.value = e;
          }
        else if (k1 == key1 && k2 == key2)
          return e;
        else 
          prev = e;
        e = next;
      }
    if (create)
      {
        Entry e = new Entry();
        /*
        Entry head1;
        if (key2 != null)
          {
            head1 = lookup(key1, null, hash1, 0, true);
            e.ref1 = head1.ref1;
            e,next1 = head1;
            head1.next1 = e;
            e1.ref1 = head1.ref1;
          }
        else
          {
            head1 = e;
            e.ref1 = key1 == null ? null : new WeakReference(key1);
            e.next1 = e;
          }
        if (key1 != null)
          {
            head2 = lookup(key2, null, hash2, 0, true);
            e.ref2 = head2.ref2;
            e,next2 = head2;
            head2.next2 = e;
            e1.ref2 = head1.ref2;
          }
        else
          {
            head2 = e;
            e.ref2 = key2 == null ? null : new WeakReference(key2);
            e.next2 = e;
          }
        e.hash = hash;
        */
        key1 = wrapReference(key1);
        key2 = wrapReference(key2);
        e.key1 = key1;
        e.key2 = key2;
        num_bindings++;
        // FIXME maybe rehash.
        e.chain = first;
        table[index] = e;
        e.value = e;
        return e;
      }
    else
      return null;
  }

  protected Object wrapReference (Object key)
  {
    /* #ifdef JAVA2 */
    return key == null || key instanceof Symbol ? key : new WeakReference(key);
    /* #else */
    // return key;
    /* #endif */
  }

  /*
  Head getHead (Object key, boolean isDim2, int hash)
  {
    int index = hash & mask;
    // FIXME: clear reference queue
    Entry first = table[index];
    for (Entry e = first;  e != null;  e = e.chain)
      {
        if (e.hash != hash || ! (e instanceof Head))
          continue;
        Head h = (Head) e;
        if (h.ref.ref() == key)
          return h;
      }
    Head h = new Head();
    h.hash = hash;
    if (isDim2)
      h.next2 = h;
    else
      h.next1 = h;
    h.chain = first;
    table[index] = h;
    h.ref = new WeakReference(key);
    return h;
  }
  */
}

class Entry
{
  //int hash;
  ///** Next in circular list with same key1. */
  //Entry next1;
  ///** Next in circular list with same key2. */
  //Entry next2;
  /** Next in list in same hash bucket. */
  Entry chain;

  /** Value, if nound.  Point to self if unbound. */
  Object value;

  Object key1, key2;

  public Object getKey1 ()
  {
    /* #ifdef JAVA2 */
    if (key1 instanceof WeakReference)
      return ((WeakReference) key1).get();
    /* #endif */
    return key1;
  }

  public Object getKey2 ()
  {
    /* #ifdef JAVA2 */
    if (key2 instanceof WeakReference)
      return ((WeakReference) key2).get();
    /* #endif */
    return key2;
  }

  public boolean matches (Object key1, Object key2)
  {
    return key1 == getKey1() && key2 == getKey2();
  }

  public Object getValue() { return value == this ? null : value; }
}

  /*
class Head extends Entry
{
  WeakRefeference ref;

  public LList make List ()
  {
    LList list = LList.Empty;
    for (Entry e = next1;// FIXME or next2
         e != null;
         e = e.next1 // FIXME or next2
           )
      {
        list = new Car (e.getKey1(),//FIXME
                          new Car (e.value, list));
      }
    return list;
  }
}
*/
