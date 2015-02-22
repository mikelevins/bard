// Copyright (c) 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import java.io.*;

/** A "namespace node" as a link in a linked list.
 *
 * The list may contain duplicates - i.e. multiple namespace bindings
 * for the same prefix but (usually) different uris.  In that case the
 * first binding "wins".  One reason for allowing duplicates it to allow
 * sharing of the lists between a child and its parent element.
 */

public final class NamespaceBinding implements Externalizable
{
  /** Namespace prefix.  An interned String.
   * A default namespace declaration is represented using null. */
  public final String getPrefix () { return prefix; }
  public final void setPrefix (String prefix) { this.prefix = prefix; }
  String prefix;

  /** Namespace uri.  An interned String.
   * The value null "undeclares" any following namespaces with the same prefix;
   * it corresponds to an empty uri as in the
   * XML Namespaces 1.1 Candidate Recommendation. */
  public final String getUri () { return uri; }
  public final void setUri (String uri) { this.uri = uri; }
  String uri;

  NamespaceBinding next;

  int depth;

  public final NamespaceBinding getNext () { return next; }
  public final void setNext (NamespaceBinding next)
  {
    this.next = next;
    this.depth = next == null ? 0 : next.depth + 1;
  }

  /** Chain the first list in front of the second list.
   * (The name {@code nconc} comes from Common Lisp.)
   */
  public final static NamespaceBinding
  nconc (NamespaceBinding list1, NamespaceBinding list2)
  {
    if (list1 == null)
      return list2;
    list1.setNext(nconc(list1.next, list2));
    return list1;
  }

  //  public NamespaceBinding () { }

  public NamespaceBinding (String prefix, String uri, NamespaceBinding next)
  {
    this.prefix = prefix;
    this.uri = uri;
    setNext(next);
  }

  public static final String XML_NAMESPACE
  = "http://www.w3.org/XML/1998/namespace";

  public static final NamespaceBinding predefinedXML
  = new NamespaceBinding("xml", XML_NAMESPACE, null);

  /** Resolve a prefix.
   * @param prefix an interned namespace prefix to search for.
   * @return a uri or null if not bound
   */
  public String resolve (String prefix)
  {
    for (NamespaceBinding ns = this;  ns != null;  ns = ns.next)
      {
	if (ns.prefix == prefix)
	  return ns.uri;
      }
    return null;
  }

  /** Resolve a prefix, in the initial part of this list.
   * @param prefix an interned namespace prefix to search for.
   * @param fencePost only search this list until then.
   * @return a uri or null if not bound
   */
  public String resolve (String prefix, NamespaceBinding fencePost)
  {
    for (NamespaceBinding ns = this;  ns != fencePost;  ns = ns.next)
      {
	if (ns.prefix == prefix)
	  return ns.uri;
      }
    return null;
  }

  public static NamespaceBinding commonAncestor (NamespaceBinding ns1,
						 NamespaceBinding ns2)
  {
    if (ns1.depth > ns2.depth)
      {
	NamespaceBinding tmp = ns1;
	ns1 = ns2;
	ns2 = tmp;
      }
    while (ns2.depth > ns1.depth)
      ns2 = ns2.next;
    while (ns1 != ns2)
      {
	ns1 = ns1.next;
	ns2 = ns2.next;
      }
    return ns1; 
  }

  /* For debugging:
  void check ()
  {
    NamespaceBinding ns = this;
    int d = depth;
    for (;;)
      {
	if (ns == null)
	  throw new Error("null ns");
	if (ns.depth != d)
	  throw new Error("bad depth "+ns.depth+" shoudl be "+d);
	ns = ns.next;
	if (ns == null && d == 0)
	  return;
	d--;
      }
  }
  */

  /** Reverse the chain, until a fencePost. */
  public NamespaceBinding reversePrefix (NamespaceBinding fencePost)
  {
    NamespaceBinding prev = fencePost;
    NamespaceBinding t = this;
    int depth = fencePost == null ? -1 : fencePost.depth;
    while (t != fencePost)
      {
	NamespaceBinding next = t.next;
	t.next = prev;
	prev = t;
	t.depth = ++depth;
	t = next;
      }
    return prev;
  }

  /** Return the number of bindings before the <code>fencePost</code>. */
  public int count (NamespaceBinding fencePost)
  {
    int count = 0;
    for (NamespaceBinding ns = this;  ns != fencePost;  ns = ns.next)
      count++;
    return count;
  }

  /** Append a new NamespaceBinding if not redundant. */
  public static NamespaceBinding maybeAdd(String prefix, String uri,
					  NamespaceBinding bindings)
  {
    if (bindings == null)
      {
	if (uri == null)
	  return bindings;
	bindings = predefinedXML;
      }
    String found = bindings.resolve(prefix);
    if (found == null ? uri == null : found.equals(uri))
      return bindings;
    return new NamespaceBinding(prefix, uri, bindings);
  }

  /** Return a String showing just a single namespace binding. */
  public String toString()
  {
    return "Namespace{"+prefix+"="+uri+", depth:"+depth+"}";
  }

  /** Return a String showing the full namespace binding list. */
  public String toStringAll()
  {
    StringBuffer sbuf = new StringBuffer("Namespaces{");
    for (NamespaceBinding ns = this;  ns != null;  ns = ns.next)
      {
	sbuf.append(ns.prefix);
	sbuf.append("=\"");
	sbuf.append(ns.uri);
	sbuf.append(ns == null ? "\"" : "\", ");
      }
    sbuf.append('}');
    return sbuf.toString();
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeUTF(prefix);
    out.writeUTF(uri);
    out.writeObject(next);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    prefix = in.readUTF();
    uri = in.readUTF();
    next = (NamespaceBinding) in.readObject();
  }

}
