// Copyright (c) 2005, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;
import java.util.*;

/** An abstract hash map from K to V.
 * The entries are represented by an Entry type parameter.
 */

public abstract class AbstractHashTable<Entry extends Map.Entry<K,V>, K, V>
  /* #ifdef JAVA2 */
  extends AbstractMap<K,V>
  /* #endif */
{
  protected Entry[] table;
  protected int mask;
  protected int num_bindings;

  /** Extract hash-code from Entry. */
  protected abstract int getEntryHashCode (Entry entry);
  /** Extract next Entry in same hash-bucket. */
  protected abstract Entry getEntryNext (Entry entry);
  /** Set next Entry in same hash-bucket. */
  protected abstract void setEntryNext (Entry entry, Entry next);
  /** Allocate Entry[n]. */
  protected abstract Entry[] allocEntries(int n);

  public static final int DEFAULT_INITIAL_SIZE = 64;

  public AbstractHashTable ()
  {
    this(DEFAULT_INITIAL_SIZE);
  }

  public AbstractHashTable (int capacity)
  {
    int log2Size = 4;
    while (capacity > (1 << log2Size))
      log2Size++;
    capacity = 1 << log2Size;
    table = allocEntries(capacity);
    mask = capacity - 1;
  }

  /** Allocate a new node in the hash table. */
  protected abstract Entry makeEntry (K key, int hash, V value);

  /** Calculate hash code of a key.
   */
  public int hash (Object key)
  {
    return key == null ? 0 : key.hashCode();
  }

  /** Map hash-code to bucket index in table. */
  protected int hashToIndex (int hash)
  {
    // Very basic re-arranging of the bits in case of poor hash.
    hash ^= hash >>> 15;
    return hash & mask;
  }

  /** True if an Entry matches a key. */
  protected boolean matches (Object key, int hash, Entry node)
  {
    return getEntryHashCode(node) == hash && matches(node.getKey(), key);
  }

  /** Compare two keys for equivalence.
   * Override this and the {@link #hash(Object)} method if you want
   * a different equivalence relation.
   */
  protected boolean matches (K key1, Object key2)
  {
    return key1 == key2 || (key1 != null && key1.equals(key2));
  }

  /** Find value for given key.  Return null if not found. */
  public V get (Object key)
  {
    return get(key, null);
  }

  /** Find Entry for given key.  Return null if not found. */
  public Entry getNode (Object key)
  {
    int hash = hash(key);
    int index = hashToIndex(hash);
    for (Entry node = table[index];
	 node != null;  node = getEntryNext(node))
      {
	if (matches(key, hash, node))
	  return node;
      }
    return null;
  }

  /** Find value for given key.  Return defaultValue if not found. */
  public V get (Object key, V defaultValue)
  {
    Entry node = getNode(key);
    return node == null ? defaultValue : node.getValue();
  }

  protected void rehash ()
  {
    Entry[] oldTable = table;
    int oldCapacity = oldTable.length;
    int newCapacity = 2 * oldCapacity;
    Entry[] newTable = allocEntries(newCapacity);
    int newMask = newCapacity - 1;
    table = newTable;
    mask = newMask;
    for (int i = oldCapacity;  --i >= 0;)
      {
        Entry chain = oldTable[i];
        if (chain != null && getEntryNext(chain) != null)
          {
            // Reverse the old chain in place, so that after re-hashing the
            // new chain has the same order.. This is useful for some
            // subclasses (specifically gnu.expr.NameLookup), and it is
            // cheap to do here where extra cache misses are unlikely.
            Entry prev = null;
            do
              {
                Entry node = chain;
                chain = getEntryNext(node);
                setEntryNext(node, prev);
                prev = node;
              }
            while (chain != null);
            chain = prev;
          }

	for (Entry element = chain;  element != null; )
	  {
	    Entry next = getEntryNext(element);
	    int hash = getEntryHashCode(element);
	    int j = hashToIndex(hash);
	    Entry head = newTable[j];
	    setEntryNext(element, head);
	    newTable[j] = element;
	    element = next;
	  }
      }
  }

  public V put (K key, V value)
  {
    return put(key, hash(key), value);
  }

  public V put (K key, int hash, V value)
  {
    int index = hashToIndex(hash);
    Entry first = table[index];
    Entry node = first;
    for (;;)
      {
	if (node == null)
	  {
            if (++num_bindings >= table.length)
              {
                rehash();
                index = hashToIndex(hash);
                first = table[index];
              }
            node = makeEntry(key, hash, value);
            setEntryNext(node, first);
            table[index] = node;
	    return null;
	  }
	else if (matches(key, hash, node))
	  {
            V oldValue = node.getValue();
            node.setValue(value);
	    return oldValue;
	  }
	node = getEntryNext(node);
      }
  }

  public V remove (Object key)
  {
    int hash = hash(key);
    int index = hashToIndex(hash);
    Entry prev = null;
    Entry node = table[index];
    while (node != null)
      {
	Entry next = getEntryNext(node);
	if (matches(key, hash, node))
	  {
	    if (prev == null)
	      table[index] = next;
	    else
	      setEntryNext(prev, next);
	    num_bindings--;
	    return node.getValue();
	  }
	prev = node;
	node = next;
      }
    return null;
  }

  public void clear ()
  {
    Entry[] t = this.table;
    for (int i = t.length;  --i >= 0; )
      {
        // Clearing the links is probably not needed,
        // but just in case someone has a reference to an entry.
        for (Entry e = t[i]; e != null; )
          {
            Entry next = getEntryNext(e);
            setEntryNext(e, null);
            e = next;
          }
        t[i] = null;
      }
    num_bindings = 0;
  }

  public int size ()
  {
    return num_bindings;
  }

  /* #ifdef JAVA2 */
  public Set<Map.Entry<K,V>> entrySet()
  {
    return new AbstractEntrySet(this);
  }

  static class AbstractEntrySet<Entry extends Map.Entry<K,V>, K, V> extends AbstractSet<Entry>
  {
    AbstractHashTable<Entry, K, V> htable;

    public AbstractEntrySet(AbstractHashTable<Entry, K, V> htable)
    {
      this.htable = htable;
    }

    public int size ()
    {
      return htable.size();
    }

    public Iterator<Entry> iterator()
    {
      return new Iterator<Entry>()
        {
          int nextIndex;
          Entry previousEntry;
          Entry currentEntry;
          Entry nextEntry;
          int curIndex = -1;

          /* Invariants:
           * currentEntry == (previousEntry == null ? htable.table[curIndex] ? previousEntry.next)
           * nextEntry == (curIndex == nextIndex ? currentEntry.next : nextIndex >= 0 ? htable.table[nextIndex] : 0)
           * nextIndex <= curIndex, except before initialization.
           */

          public boolean hasNext()
          {
            if (curIndex < 0)
              { // initialize.
                nextIndex = htable.table.length;
                curIndex = nextIndex;
                advance();
              }
            return nextEntry != null;
          }
          private void advance ()
          {
            while (nextEntry == null && --nextIndex >= 0)
              {
                nextEntry = htable.table[nextIndex];
              }
          }

          public Entry next()
          {
            if (nextEntry == null)
              throw new NoSuchElementException();
            previousEntry = currentEntry;
            currentEntry = nextEntry;
            curIndex = nextIndex;
            nextEntry = htable.getEntryNext(currentEntry);
            advance();
            return currentEntry;
          }
          public void remove ()
          {
            if (previousEntry == currentEntry)
              throw new IllegalStateException();
            if (previousEntry == null)
              htable.table[curIndex] = nextEntry;
            else
              htable.setEntryNext(previousEntry, nextEntry);
            htable.num_bindings--;
            previousEntry = currentEntry; // As a marker to detect IllegalStateException.
          }
        };
    }
  }
  /* #endif */
}
