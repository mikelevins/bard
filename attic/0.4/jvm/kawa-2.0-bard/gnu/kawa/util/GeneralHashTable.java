// Copyright (c) 2005, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;

/** A generic hash table.
 * Supports deletions, and re-allocates the table when too big.
 * The equivalence relation can be customized. */

public class GeneralHashTable<K,V>
  extends AbstractHashTable<HashNode<K,V>,K,V>
{
  public GeneralHashTable ()
  {
  }

  public GeneralHashTable (int capacity)
  {
    super(capacity);
  }

  protected int getEntryHashCode (HashNode<K,V> entry) { return entry.keyHash; }
  protected HashNode<K,V> getEntryNext (HashNode<K,V> entry) { return entry.next; }
  protected void setEntryNext (HashNode<K,V> entry, HashNode<K,V> next) { entry.next = next; }
  protected HashNode<K,V>[] allocEntries(int n) { return (HashNode<K,V>[]) new HashNode[n]; }

  /** Allocate a new node in the hash table. */
  protected HashNode<K,V> makeEntry (K key, int hash, V value)
  {
    return new HashNode<K,V>(key, value, hash);
  }

  /** This override helps Kawa type-inference - for example in srfi69.scm. */
  public HashNode<K,V> getNode (Object key)
  {
    return super.getNode(key);
  }

}
