package gnu.kawa.models;

/** A "vertical box" container. */

public class Column extends Box
  implements Viewable, java.io.Serializable
{
  public int getAxis()
  {
    return 1;
  }
}
