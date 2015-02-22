package gnu.kawa.xslt;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;

/** Manages the set of xslt templates that have the same 'mode'. */

public class TemplateTable
{
  /** The "mode" parameter of xsl:template. */
  Symbol name;

  static final TemplateTable nullModeTable = new TemplateTable(XSLT.nullMode);

  public TemplateTable(Symbol mode)
  {
    this.name = mode;
  }

  static TemplateTable getTemplateTable(Symbol name)
  {
    if (name == XSLT.nullMode)
      return nullModeTable;
    return null;  // FIXME
  }

  TemplateEntry entries;  // For now - later use HashTable.

  public void enter(String pattern, double priority, Procedure procedure)
  {
    TemplateEntry entry = new TemplateEntry();
    entry.procedure = procedure;
    entry.priority = priority;
    entry.pattern = pattern;
    entry.next = entries;
    entries = entry;
  }

  public Procedure find (String name)
  {
    for (TemplateEntry entry = entries;  entry != null;  entry = entry.next)
      {
	if (entry.pattern.equals(name))
	  return entry.procedure;
      }
    return null;
  }
}

/** Each xsl:template creates a TemplateEntry. */

class TemplateEntry
{
  Procedure procedure;
  double priority;
  String pattern;
  TemplateEntry next;
}
