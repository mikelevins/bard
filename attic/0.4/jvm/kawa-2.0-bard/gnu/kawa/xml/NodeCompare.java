package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

/** Compare nodes for document order.
 * Implements the XQuery operators '<<', '>>', 'is', 'isnot'.
 */

public class NodeCompare extends Procedure2
{
  // Return codes from Numeric.compare:
  static final int RESULT_GRT = 1;
  static final int RESULT_EQU = 0;
  static final int RESULT_LSS = -1;

  // One flag bit for each of the above RESULT_XXX codes:
  static final int TRUE_IF_GRT = 1 << (RESULT_GRT + 3);
  static final int TRUE_IF_EQU = 1 << (RESULT_EQU + 3);
  static final int TRUE_IF_LSS = 1 << (RESULT_LSS + 3);
  int flags;

  // The funny name of the static fields and methods correspond to
  // the name mangling scheme defined in Compilation.mangleName.
  // They can therefor be statically resolved by the compiler.

  public static final NodeCompare $Eq  = make("is",TRUE_IF_EQU);
  public static final NodeCompare $Ne  = make("isnot",TRUE_IF_LSS|TRUE_IF_GRT);
  public static final NodeCompare $Gr  = make(">>",TRUE_IF_GRT);
  public static final NodeCompare $Ls  = make("<<",TRUE_IF_LSS);

  public static NodeCompare make(String name, int flags)
  {
    NodeCompare proc = new NodeCompare();
    proc.setName(name);
    proc.flags = flags;
    return proc;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg1 == null || arg2 == null)
      return null;
    if (arg1 == Values.empty)
      return arg1;
    if (arg2 == Values.empty)
      return arg2;

    AbstractSequence seq1, seq2;
    int ipos1, ipos2;
    if (arg1 instanceof AbstractSequence)
      {
	seq1 = (AbstractSequence) arg1;
	ipos1 = seq1.startPos();
      }
    else
      {
	try
	  {
	    SeqPosition spos = (SeqPosition) arg1;
	    seq1 = spos.sequence;
	    ipos1 = spos.getPos();
	  }
	catch (ClassCastException ex)
	  {
	    throw WrongType.make(ex, this, 1, arg1);
	  }
      }
    if (arg2 instanceof AbstractSequence)
      {
	seq2 = (AbstractSequence) arg2;
	ipos2 = seq2.startPos();
      }
    else
      {
	try
	  {
	    SeqPosition spos = (SeqPosition) arg2;
	    seq2 = spos.sequence;
	    ipos2 = spos.getPos();
	  }
	catch (ClassCastException ex)
	  {
	    throw WrongType.make(ex, this, 2, arg2);
	  }
      }

    int comp;
    if (seq1 == seq2)
      comp = seq1.compare(ipos1, ipos2);
    else if (this == $Eq)
      return Boolean.FALSE;
    else if (this == $Ne)
      return Boolean.TRUE;
    else
      comp = seq1.stableCompare(seq2);
    if ((1 << (3 + comp) & flags) != 0)
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }
}
