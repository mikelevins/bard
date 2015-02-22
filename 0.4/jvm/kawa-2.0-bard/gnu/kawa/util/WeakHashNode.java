package gnu.kawa.util;
import java.util.*;

public class WeakHashNode<K,V>
  /* #ifdef JAVA2 */
  extends java.lang.ref.WeakReference<K>
  implements Map.Entry<K,V>
  /* #endif */
{
  public WeakHashNode<K,V> next;
  public int hash;
  public V value;

  public WeakHashNode(K key,
                      /* #ifdef JAVA2 */
                      java.lang.ref.ReferenceQueue<K> q,
                      /* #endif */
                      int hash)
  {
    /* #ifdef JAVA2 */
    super(key, q);
    /* #else */
    // this.key = key;
    /* #endif */
    this.hash = hash;
  }
  /* #ifndef JAVA2 */
  // K key;
  // public K get() { return key; }
  /* #endif */

  public K getKey ()
  {
    return get();
  }

  public V getValue ()
  {
    return value;
  }

  public V setValue (V value)
  {
    V old = this.value;
    this.value = value;
    return old;
  }
}
