package gnu.text;
import gnu.lists.Consumer;

public interface Printable
{
  // Tempting to do: void print (Appendable out);
  // in the JAVA5 case.  However, then we have to deal
  // with the complication that the append methods
  // in Appendable are specified as 'throws IOException'.
  // Sigh.  Maybe later.
  void print (Consumer out);
}
