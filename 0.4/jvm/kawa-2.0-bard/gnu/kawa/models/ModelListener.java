package gnu.kawa.models;

/** Implemented by an object that wants to be notified when a model changes.
 * (Similar to a java.beans.PropertyChangeListener; should perhaps use that.
 * A disadvantage of PropertyChangeListener is that it requires allocating a
 * PropertyChangeEvent on each change but maybe "compatibility" is worth it.) */

public interface ModelListener
{
  public void modelUpdated (Model model, Object key);
}
