package gnu.lists;

/**
 * This singleton class represents an empty list.
 * Using a separate class easier improved type information in error messages.
 *
 * @author Charles Turner
 * @since 16/04/2011
 * @see gnu.lists.LList kawa.standard.Scheme
 */

public class EmptyList extends LList
{
  public static final EmptyList emptyList = new EmptyList();
  
  private EmptyList()
  {
  }
}

