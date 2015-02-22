package gnu.mapping;
// FIXME will probably repace by some general "attribute" mechanism.

public interface Named
{
  public String getName();

  Object getSymbol ();

  public void setName (String name);
}
