package gnu.kawa.models;


public class WeakListener extends java.lang.ref.WeakReference
{
  public WeakListener (Object referent)
  {
    super(referent);
  }

  public WeakListener (Object referent, WeakListener next)
  {
    super(referent);
    this.next = next;
  }
  WeakListener next;

  public void update (Object view, Model model, Object key)
  {
    ((ModelListener) view).modelUpdated(model, key);
  }
}
