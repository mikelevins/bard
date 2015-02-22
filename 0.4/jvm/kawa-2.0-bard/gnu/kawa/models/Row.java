package gnu.kawa.models;

/** A "horizontal box" container. */

public class Row extends Box implements Viewable
{
  public int getAxis()
  {
    return 0;
  }
}
