// Copyright (c) 1997, 2004, 2008, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.ArrayList;

/**
 * A Label represents a location in a Code attribute.
 */

public class Label {

  /** Offset of definition in the fixup_offsets and fixup_labels arrays.
   * The offset corresponds to the fixup itself. */
  int first_fixup;

  /** The PC of where the label is, or -1 if not yet defined.
   * The value -2 means don't generate a StackMapTable entry.
   * This PC may be tentative if we later run processFixups.
   * The offset in the code array is cattr.fixupOffset(first_fixup). */
  int position;

  boolean needsStackMapEntry;

  // FIXME Probably more efficient to have a single array:
  // local-types followed by stack-types.  We'd need an extra short field.
  Type[] stackTypes;
  Type[] localTypes;

  public final boolean defined () { return position >= 0; }

  public Label ()
  {
    this(-1);
  }

  public Label (CodeAttr code)
  {
    this(-1);
  }

  public Label (int position)
  {
    this.position = position;
  }

    public boolean isUsed() { return stackTypes != null; }

  Type mergeTypes (Type t1, Type t2)
  {
    if ((t1 instanceof PrimType) != (t2 instanceof PrimType))
      return null;
    return Type.lowestCommonSuperType(t1, t2);
  }

  void setTypes (Type[] locals, int usedLocals,
                 Type[] stack, int usedStack)
  {
    for (; usedLocals > 0; usedLocals--)
      {
        Type last = locals[usedLocals-1];
        if (last != null)
          break;
      }
    if (stackTypes == null)
      {
        if (usedStack == 0)
          stackTypes = Type.typeArray0;
        else
          {
            stackTypes = new Type[usedStack];
            System.arraycopy(stack, 0, stackTypes, 0, usedStack);
          }
        if (usedLocals == 0)
            localTypes = Type.typeArray0;
        else
          {
            localTypes = new Type[usedLocals];
            System.arraycopy(locals, 0, localTypes, 0, usedLocals);
          }
      }
    else
      {
        int SP = usedStack;
        int slen = stackTypes.length;
        if (SP != slen)
          throw new InternalError("inconsistent stack length");
        for (int i = 0; i < SP; i++)
          {
            stackTypes[i] = mergeTypes(stackTypes[i], stack[i]);
          }
        for (int i = 0; i < localTypes.length;  i++)
          {
            mergeLocalType(i, i < usedLocals ? locals[i] : null);
          }
      }
  }

  public void setTypes (CodeAttr code)
  {
    addTypeChangeListeners(code);
    if (stackTypes != null && code.SP != stackTypes.length)
      throw new InternalError();
    setTypes(code.local_types,
             code.local_types == null ? 0 : code.local_types.length,
             code.stack_types,
             code.SP);
  }

  public void setTypes (Label other)
  {
    setTypes(other.localTypes, other.localTypes.length,
             other.stackTypes, other.stackTypes.length);
  }

  private void mergeLocalType (int varnum, Type newType)
  {
    if (varnum < localTypes.length)
      {
        Type oldLocal = localTypes[varnum];
        Type newLocal = mergeTypes(oldLocal, newType);
        if (newLocal != oldLocal)
          {
            localTypes[varnum] = newLocal;
            notifyTypeChangeListeners(varnum, newLocal);
          }
      }
  }

  private void notifyTypeChangeListeners (int varnum, Type newType)
  {
    Object[] arr = typeChangeListeners;
    if (arr == null || arr.length <= varnum)
      return;
    Object listeners = arr[varnum];
    if (listeners == null)
      return;
    if (listeners instanceof Label)
      ((Label) listeners).mergeLocalType(varnum, newType);
    else
      {
        for (Label listener : (ArrayList<Label>) listeners)
          listener.mergeLocalType(varnum, newType);
      }
    if (newType == null)
      arr[varnum] = null;
  }

  /** Map from Variable number to set of listeners.
   * When {@code this.localTypes[varnum]} is invalidated, then we also
   * need to invalidate that variable in all the listeners.
   * The type is actually a {@code Union<Label,ArrayList<Label>>[]}.
   */
  private Object[] typeChangeListeners;

  void addTypeChangeListener (int varnum, Label listener)
  {
    Object[] arr = typeChangeListeners;
    if (arr == null)
      typeChangeListeners = arr = new Object[varnum + 10];
    else if (arr.length <= varnum)
      {
        arr = new Object[varnum + 10];
        System.arraycopy(typeChangeListeners, 0, arr, 0, typeChangeListeners.length);
        typeChangeListeners = arr;
      }
    Object set = arr[varnum];
    if (set == null)
      arr[varnum] = listener;
    else
      {
        ArrayList<Label> list;
        if (set instanceof Label)
          {
            list = new ArrayList<Label>();
            list.add((Label) set);
            arr[varnum] = list;
          }
        else
          list = (ArrayList<Label>) set;
        list.add(listener);
      }
  }

  void addTypeChangeListeners (CodeAttr code)
  {
    if (code.local_types != null && code.previousLabel != null)
      {
        int len = code.local_types.length;
        for (int varnum = 0;  varnum < len;  varnum++)
          {
            if (code.local_types[varnum] != null
                && (code.varsSetInCurrentBlock == null
                    || code.varsSetInCurrentBlock.length <= varnum
                    || ! code.varsSetInCurrentBlock[varnum]))
              code.previousLabel.addTypeChangeListener(varnum, this);
          }
      }
  }
  /**
   * Define the value of a label as having the current location.
   * @param code the "Code" attribute of the current method
   */
  public void defineRaw (CodeAttr code)
  {
    if (position >= 0)
      throw new Error ("label definition more than once");
    position = code.PC;
    first_fixup = code.fixup_count;
    if (first_fixup >= 0)
      code.fixupAdd(CodeAttr.FIXUP_DEFINE, this);
  }

  /**
   * Define the value of a label as having the current location.
   * @param code the "Code" attribute of the current method
   */
  public void define (CodeAttr code)
  {
    if (code.reachableHere())
      {
        setTypes(code);
      }
    else if (localTypes != null)
      {
        for (int i = localTypes.length; --i >= 0; )
          {
            if (localTypes[i] != null
                && (code.locals.used == null || code.locals.used[i] == null))
              {
                localTypes[i] = null;
              }
          }
      }
    code.previousLabel = this;
    code.varsSetInCurrentBlock = null; // FIXME - zero out instead.
    defineRaw(code);
    if (localTypes != null)
      // Copy merged type back to current state.
      code.setTypes(this);
    code.setReachable(true);
  }

  /* DEBUG
  int id = ++counter;
  static int counter;
  public String toString() { return "Label#"+id+"-pos:"+position; }
  */
}
