// Copyright (c) 2011  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;
import java.util.HashMap;
import gnu.math.BitOps;
import gnu.math.IntNum;
import java.util.*;

/** Used for SSA (Static Single Assignment) analysis.
 */

public class VarValueTracker
{
  InlineCalls visitor;

  public VarValueTracker (InlineCalls visitor)
  {
    this.visitor = visitor;
    declValueUsage = new HashMap<Declaration,IntNum>();
  }

  public VarValueTracker (VarValueTracker outer)
  {
    this.outer = outer;
    this.declValueUsage = outer.declValueUsage;
  }

  /** Used to keep track of ValueSource setters that may reach here.
   * If only a single ValueSource value can reach the current
   * position, then the IntNum value is the boolean inverse
   * of the index into the Declarations values array.
   * (I.e. the integer is a negative value.)
   * Otherwise, the IntNum is a bitmap, whose bit index is an index
   * into the declarations's values array.  The index is offset by 1,
   * since the index 0 is used for "uninitialized".  A bit is set
   * if the corresponding ValueSource may flow to this position.
   */
  HashMap<Declaration,IntNum> declValueUsage;

  /** Handle fork/merge (as in IfExp) of declValueUsage values.
   * The value for a {@code Declaration d} is a 2-element array
   * {@code [old, result]}:
   * The value {@code old} of {@code declValueUsage.get(d)} when
   * entering the fork.
   * The value {@code result} is the union of the values from previous forks.
   */
  HashMap<Declaration,IntNum[]> forkValues;

  int forkBranchNumber;

  VarValueTracker outer;

  /** Called when starting a new fork, for example a new {@code IfExp}. */
  public static void forkPush (InlineCalls visitor)
  {
    VarValueTracker oldTracker = visitor.valueTracker;
    VarValueTracker newTracker = new VarValueTracker(oldTracker);
    visitor.valueTracker = newTracker;
    newTracker.forkValues = new HashMap<Declaration,IntNum[]>();
  }

  /** Called when switching to a new branch of fork, for example the {@code else} clause of a new {@code IfExp}. */
  public void forkNext ()
  {
    forkBranchNumber++;
    for (Map.Entry<Declaration,IntNum[]> entry : forkValues.entrySet())
      {
        Declaration decl = entry.getKey();
        IntNum[] vals = entry.getValue();
        IntNum cur = declValueUsage.get(decl);
        vals[1] = sourceUnion(vals[1], cur);
        declValueUsage.put(decl, vals[0]);
      }

    lambdasCheckedForUninitializedVariables = null;
  }

  public static void forkPop (InlineCalls visitor)
  {
    VarValueTracker innerTracker = visitor.valueTracker;
    VarValueTracker outerTracker = innerTracker.outer;
    HashMap<Declaration,IntNum> declValueUsage = innerTracker.declValueUsage;
    visitor.valueTracker = outerTracker;
    for (Map.Entry<Declaration,IntNum[]> entry :  innerTracker.forkValues.entrySet())
      {
        Declaration decl = entry.getKey();
        IntNum[] vals = entry.getValue();
        IntNum newSource = sourceUnion(vals[1], declValueUsage.get(decl));
        outerTracker.noteSet(decl, newSource);
      }
  }

  static IntNum sourceAsMask (IntNum val)
  {
    if (val.isNegative())
      {
        int index = ~ val.intValue();
        val = IntNum.shift(IntNum.one(), index+1);
      }
    return val;
  }

  static IntNum sourceUnion(IntNum set1, IntNum set2)
  {
    if (set1 == set2)
      return set1;
    if (set2.isZero())
      return set1;
    if (set1.isZero())
      return set2;
    return BitOps.ior(sourceAsMask(set1), sourceAsMask(set2));
  }

  static boolean maybeUninitialized (IntNum set)
  {
    return ! set.isNegative() && set.isOdd();
  }

  /** Note that decl is uninitialized. */
  public void noteUnitialized (Declaration decl)
  {
    if (decl.values != Declaration.unknownValueValues) {
      declValueUsage.put(decl, IntNum.one());
    }
  }

  public void noteUnitialized (ScopeExp scope)
  {
    for (Declaration decl = scope.firstDecl();  decl != null;
         decl = decl.nextDecl())
      noteUnitialized(decl);
  }

  public void noteSet (Declaration decl, IntNum source)
  {
    IntNum curSource = declValueUsage.get(decl);
    if (forkValues != null && curSource != null)
      {
        IntNum[] vals = forkValues.get(decl);
        if (vals == null)
          {
            vals = new IntNum[2];
            vals[0] = curSource;
            vals[1] = forkBranchNumber==0 ? IntNum.zero() : curSource;
            forkValues.put(decl, vals);
          }
      }
    declValueUsage.put(decl, source);
  }

  Set<LambdaExp> lambdasCheckedForUninitializedVariables;

  /** Look for possible-uninitialized variable references in {@code lexp}.
   * Also check functions that may be called from {@code lexp}.
   */
  void checkUninitializedVariables (LambdaExp lexp,
                                    ReferenceExp referrer,
                                    Stack<ReferenceExp> callers)
  {
    if (lambdasCheckedForUninitializedVariables == null)
      lambdasCheckedForUninitializedVariables = new HashSet<LambdaExp>();
    else if (lambdasCheckedForUninitializedVariables.contains(lexp))
      return;
    lambdasCheckedForUninitializedVariables.add(lexp);
    if (callers == null)
      callers = new Stack<ReferenceExp>();
    callers.push(referrer);
    for (ReferenceExp rexp = lexp.siblingReferences;  rexp != null;
         rexp = rexp.siblingReferencesNext)
      {
        Declaration decl = rexp.getBinding();
        LambdaExp lvalue = decl.getLambdaValue();
        if (lvalue != null)
          {
            checkUninitializedVariables(lvalue, rexp, callers);
          }
        else
          { 
            IntNum vals = declValueUsage.get(decl);
            if (vals != null && maybeUninitialized(vals)
                && ! decl.getFlag(Declaration.MAYBE_UNINITIALIZED_ACCESS))
              {
                Compilation comp = visitor.getCompilation();
                comp.error('w', "variable '"+rexp.getName()+"' may be uninitialized here", rexp);
                if (callers != null)
                  {
                    int i = callers.size();
                    while (--i >= 0)
                      comp.error('w', "- because of possible call here of function "+callers.get(i).getName(), callers.get(i));                      
                  }
                decl.setFlag(Declaration.MAYBE_UNINITIALIZED_ACCESS);
              }
          }
      }
    callers.pop();
  }
}
