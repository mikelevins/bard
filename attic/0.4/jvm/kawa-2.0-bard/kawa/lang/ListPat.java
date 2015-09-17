package kawa.lang;
import gnu.lists.*;
import gnu.text.*;

/** Match a list whose length in in the range [min_length..max_length]. */

public class ListPat extends Pattern
{
  /** Minimun length of list that will match. */
  int min_length;
  /** Maximum length of list that will match. */
  int max_length;
  Object default_value;

  public ListPat (int len) { min_length = len;  max_length = len; }
  public ListPat (int min, int max) { min_length = min;  max_length = max; }
  public ListPat (int min, int max, Object default_val)
  { min_length = min;  max_length = max; default_value = default_val; }

  public static boolean match (int min, int max, Object default_val,
                               Object obj, Object[] vars, int start_vars)
  {
    int i;
    for (i = 0; i < max; i++)
      {
	if (obj instanceof Pair)
	  {
	    Pair p = (Pair)obj;
	    vars[start_vars + i] = p.getCar();
	    obj = p.getCdr();
	  }
	else if (i < min)
	  return false;
	else
	  break;
      }
    if (i == max && obj != LList.Empty)
      return false;
    for ( ; i < max; i++)
      vars[start_vars + i] = default_val;
    return true;
  }

  /**
   * Succeeds if obj is a list of length [min..max].
   * @param obj the object to match against
   * @return true iff the match succeeded
   * On success, max_length values from the elements of the list are placed
   * in vars (starting at start_vars); if obj is shorter, missing elements
   * are set to default_value.
   */
  public static Object[] match(int min, int max, Object default_val,
                               Object obj)
  {
    Object[] vars = new Object[max];
    return match(min, max, default_val, obj, vars, 0) ? vars : null;
  }

  /**
   * Succeeds if obj is a list of length [min_length..max_length].
   * @param obj the object to match against
   * @return null on failure, or an array of bound pattern variables:
   * max_length values from the elements of the list are placed
   * in the result; if obj is shorter, missing elements
   * are set to default_value.
   */
  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    return match(min_length, max_length, default_value,
                 obj, vars, start_vars);
  }

  public int varCount () { return max_length; }

  public void print (Consumer out)
  {
    out.write("#<list-pattern min:");
    out.write(Integer.toString(min_length));
    out.write(" max:");
    out.write(Integer.toString(max_length));
    out.write(" default:");
    ReportFormat.print(default_value, out);
    out.write('>');
  }
}
