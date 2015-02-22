// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import gnu.mapping.Symbol;

/** A FilterConsumer that only passes through matching children.
 */

public class NamedChildrenFilter extends FilterConsumer
{
  String namespaceURI;
  String localName;

  int level;
  int matchLevel;

  public static NamedChildrenFilter
  make (String namespaceURI, String localName, Consumer out)
  {
    return new NamedChildrenFilter(namespaceURI, localName, out);
  }

  public NamedChildrenFilter (String namespaceURI, String localName,
			      Consumer out)
  {
    super(out);
    this.namespaceURI = namespaceURI;
    this.localName = localName;
    skipping = true;
  }

  public void startDocument()
  {
    level++;
    super.startDocument();
  }

  public void endDocument()
  {
    level--;
    super.endDocument();
  }

  public void startElement (Object type)
  {
    if (skipping && level == 1 // && axis is child::
	// || axis is descdendent-or-self::
	// || level >= 1 && axis is descdendent
	)
      {
	String curNamespaceURI;
	String curLocalName;
	if (type instanceof Symbol)
	  {
	    Symbol qname = (Symbol) type;
	    curNamespaceURI = qname.getNamespaceURI();
	    curLocalName = qname.getLocalName();
	  }
	else
	  {
	    curNamespaceURI = "";
	    curLocalName = type.toString().intern();  // FIXME
	  }
	if ((localName == curLocalName || localName == null)
	    && (namespaceURI == curNamespaceURI || namespaceURI == null))
	  {
	    skipping = false;
	    matchLevel = level;
	  }
      }

    super.startElement(type);
    level++;
  }

  public void endElement ()
  {
    level--;
    super.endElement();
    if (! skipping && matchLevel == level)
      skipping = true;
  }

  public void writeObject(Object val)
  {
    if (val instanceof SeqPosition)
      {
	SeqPosition pos = (SeqPosition) val;
	if (pos.sequence instanceof TreeList)
	  {
	    ((TreeList) pos.sequence).consumeNext(pos.ipos, this);
	    return;
	  }
      }
    if (val instanceof Consumable)
      ((Consumable) val).consume(this);
    else
      super.writeObject(val);
  }
}
