package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.io.OutPort;
import gnu.lists.*;
import java.util.Vector;
import gnu.kawa.functions.DisplayFormat;
import gnu.text.*;
import java.io.*;

/** This encodes a pattern from a Scheem syntax-case or syntax-rules. */

public class SyntaxPattern extends Pattern implements Externalizable
{
  /** An encoding of the pattern in a compact form.
   * This is a sequence of "matching instructions".  These have a 3-bit
   * "opcode", which is one of the <code>MATCH_XXX</code> cosntants.
   * The leaves 13 bits available as an operand; if that isn't enough the
   * <code>MATCH_WIDE</code> "instruction" can be used to modify the
   * following instruction. */
  String program;

    /** A string (if non-null) of the form FILENAME:LINENUMBER. */
    String fileLine;

  public static final SimpleSymbol underscoreSymbol = Symbol.valueOf("_");

  /** This 3-bit "opcode" is used for shorter operand-less instructions. */
  static final int MATCH_MISC = 0;

  /** Matches <code>List.Empty</code>. */
  static final int MATCH_NIL = (1<<3)+MATCH_MISC;

  /** Matches a vector (FVector).
   * Matches a vector v if the following list pattern (at pc+1)
   * matches (vector->list v). */
  static final int MATCH_VECTOR = (2<<3)+MATCH_MISC;

  /** Match anything and ignore it. */
  static final int MATCH_IGNORE = (3<<3)+MATCH_MISC;

  /** The instruction <code>8*i+MATCH_WIDE</code> is a prefix.
   * It causes <code>i&lt;&lt;13</code> to be added to the parameter
   * (<code>i</code>) of the following instruction. */
  static final int MATCH_WIDE = 1;

  /** The instruction <code>8*i+MATCH_EQUALS</code> matches the literal values literals[i]. */
  static final int MATCH_EQUALS = 2;

  /** The instruction <code>8*i+MATCH_ANY</code> matches any form,
   * It sets <code>vars[i]</code> to the matched form. */
  static final int MATCH_ANY = 3;

  /** The instruction <code>8*i+MATCH_PAIR</code> matches a Pair.
   * Its <code>car</code> must match the pattern at <code>pc+1</code>, while
   * its <code>cdr</code> must match the mattern at <code>pc+1+i</code>. */
  static final int MATCH_PAIR = 4;

  /** The instruction <code>8*i+MATCH_LREPEAT</code> matches a repeated
   * pattern.  The repeated sub-pattern starts at <code>pc+1</code>,
   * and is <code>i</code> chars long.  Following that (at <code>pc+1+i</code>)
   * is the index of the first pattern variable in the sub-pattern,
   * followed by the count of pattern variables in the sub-pattern.
   * (Both are shifted left by 3 in case we need <code>MATCH_WIDE</code>).
   * This is followed either by a <code>MATCH_NIL</code> (in which case
   * all remaining elements must match the repeated sub-pattern),
   * or by a <code>MATCH_LENGTH</code> (which must match the tail). */
  static final int MATCH_LREPEAT = 5;

  /** The instruction <code>8*i+MATCH_LENGTH</code> matches a pure list
   * of length <code>2*i</code> or an impure list of <code>2*i+1</code> pairs.
   * It is followed by a pattern which must also match. */
  static final int MATCH_LENGTH = 6;

  /** The instruction <code>8*i+MATCH_CAR</code> matches the car of a Pair,
   * It sets <code>vars[i]</code> to the Pair itself. */
  static final int MATCH_ANY_CAR = 7;

  Object[] literals;
  int varCount;

  public int varCount() { return varCount; }

    /** Control logging to standard error on successful pattern match. */
    public static boolean printSyntaxPatternMatch;

    public boolean match(Object obj, Object[] vars, int start_vars) {
        boolean r = match(obj, vars, start_vars, 0, null);
        if (printSyntaxPatternMatch && r) {
            OutPort log = OutPort.errDefault();
            log.startLogicalBlock("{syntax-pattern ", false, "}");
            log.setIndentation(-14, false);
            if (fileLine != null) {
                log.print(fileLine);
            }
            log.writeSpaceLinear();
            log.print("match ");
            DisplayFormat.schemeWriteFormat.writeObject(obj, log);
            if (r) {
                log.print(" -> vars: ");
                for (int i = start_vars;  i < vars.length;  i++) {
                    log.writeSpaceLinear();
                    log.print(i);
                    log.print(": ");
                    DisplayFormat.schemeWriteFormat.writeObject(vars[i], log);
                }
            }
            else
                log.println(" -> failed");
            log.endLogicalBlock("}");
            log.println();
        }
        return r;
  }

  public SyntaxPattern (String program, Object[] literals,
                        int varCount, String fileLine)
  {
    this.program = program;
    this.literals = literals;
    this.varCount = varCount;
    this.fileLine = fileLine;
  }

  public SyntaxPattern (Object pattern,
			Object[] literal_identifiers, Translator tr)
  {
    this(new StringBuilder(), pattern,
	 null, SyntaxRule.dots3Symbol, literal_identifiers, tr);
  }

  SyntaxPattern(StringBuilder programbuf, Object pattern, SyntaxForm syntax,
                Object ellipsis, Object[] literal_identifiers,
		Translator tr)
  {
    Vector literalsbuf = new Vector();
    translate(pattern, programbuf, ellipsis,
	      literal_identifiers, 0, literalsbuf, null, '\0', tr);
    program = programbuf.toString();
    literals = new Object[literalsbuf.size()];
    literalsbuf.copyInto(literals);
    varCount = tr.patternScope.pattern_names.size();
    String filename = tr.getFileName();
    int fileslash = filename.replace(File.separatorChar, '/').lastIndexOf('/');
    fileLine = fileslash >= 0 ? filename.substring(fileslash+1) : filename;
    int line = tr.getLineNumber();
    if (line > 0)
        fileLine = fileLine + ':' + line;
    /* DEBUGGING:
    System.err.print("{translated pattern");
    Macro macro = tr.currentMacroDefinition;
    if (macro != null)
      {
	System.err.print(" for ");
	System.err.print(macro);
      }
    String file = filename;
    if (file != null)
      {
	System.err.print(" file=");
	System.err.print(file);
      }
    if (line > 0)
      {
	System.err.print(" line=");
	System.err.print(line);
      }
    System.err.print(" vars=");
    System.err.print(varCount);
    System.err.println(':');
    disassemble();
    */
  }

  public void disassemble ()
  {
    disassemble(OutPort.errDefault(), (Translator) Compilation.getCurrent(),
		0, program.length());
  }

  public void disassemble (java.io.PrintWriter ps, Translator tr)
  {
    disassemble(ps, tr, 0, program.length());
  }

  void disassemble (java.io.PrintWriter ps, Translator tr, int start, int limit)
  {
    Vector pattern_names = null;
    if (tr != null && tr.patternScope != null)
      pattern_names = tr.patternScope.pattern_names;
    int value = 0;
    for (int i = start;  i < limit;  )
      {
	char ch = program.charAt(i);
	ps.print(" " + i + ": " + (int)ch);
	i++;
	int opcode = ch & 7;
	value = (value << 13) | (ch >> 3);
	switch (opcode)
	  {
	  case MATCH_WIDE:
	    ps.println(" - WIDE "+value);
	    continue;
	  case MATCH_EQUALS:
	    ps.print(" - EQUALS["+value+"]");
	    if (literals != null && value >= 0 && value < literals.length)
	      ps.print(literals[value]);
	    ps.println();
	    break;
	  case MATCH_ANY:
	  case MATCH_ANY_CAR:
	    ps.print((opcode == MATCH_ANY ? " - ANY[" : " - ANY_CAR[")
		     +value+"]");
	    if (pattern_names != null
		&& value >= 0 && value < pattern_names.size())
	      ps.print(pattern_names.elementAt(value));
	    ps.println();
	    break;
	  case MATCH_PAIR:
	    ps.println(" - PAIR["+value+"]");
	    break;
	  case MATCH_LREPEAT:
	    ps.println(" - LREPEAT["+value+"]");
	    disassemble(ps, tr, i, i+value);
	    i += value;
	    ps.println(" " + i + ": - repeat first var:"+(program.charAt(i++)>>3));
	    ps.println(" " + i + ": - repeast nested vars:"+(program.charAt(i++)>>3));
	    break;
	  case MATCH_LENGTH:
	    ps.println(" - LENGTH "+(value>>1)+" pairs. "
		       + (((value&1)==0?"pure list":"impure list")));
	    break;
	  case MATCH_MISC:
	    ps.print("[misc ch:"+(int)ch+" n:"+(int)(MATCH_NIL)+"]");
	    if (ch == MATCH_NIL)
	      {
		ps.println(" - NIL");
		break;
	      }
	    if (ch == MATCH_VECTOR)
	      {
		ps.println(" - VECTOR");
		break;
	      }
	    if (ch == MATCH_IGNORE)
	      {
		ps.println(" - IGNORE");
		break;
	      }
	  default:
	    ps.println(" - "+opcode+'/'+value);
	    break;
	  }
	value = 0;
      }
  }



  /**
   * @param context 'V' : vector elements; 'P' : car of Pair; '\0' : other.
   */
  void translate (Object pattern, StringBuilder program,
		  Object ellipsis, Object[] literal_identifiers, int nesting,
		  Vector literals, SyntaxForm syntax,
		  char context,
		  Translator tr)
  {
    PatternScope patternScope = tr.patternScope;
    Vector patternNames = patternScope.pattern_names;
    for (;;)
      {
	while (pattern instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) pattern;
	    pattern = syntax.getDatum();
	  }
	if (pattern instanceof Pair)
	  {
	    Object savePos = tr.pushPositionOf(pattern);
	    try
	      {
		int start_pc = program.length();
		program.append((char) MATCH_PAIR);
		Pair pair = (Pair) pattern;
		SyntaxForm car_syntax = syntax;
		Object next = pair.getCdr();
		while (next instanceof SyntaxForm)
		  {
		    syntax = (SyntaxForm) next;
		    next = syntax.getDatum();
		  }
		boolean repeat = false;
                if (next instanceof Pair) {
                    Pair nextPair = (Pair) next;
                    Object nextCar = nextPair.getCar();
                    if (literalIdentifierEq(nextCar, syntax == null ? null : syntax.getScope(), ellipsis, null)) {
		    repeat = true;
		    next = nextPair.getCdr();
		    while (next instanceof SyntaxForm)
		      {
			syntax = (SyntaxForm) next;
			next = syntax.getDatum();
		      }
		  }
                }

		int subvar0 = patternNames.size();
		if (context == 'P')
		  context = '\0';
		translate(pair.getCar(), program,
                          ellipsis, literal_identifiers,
			  repeat ? nesting + 1 : nesting,
			  literals, car_syntax,
			  context == 'V' ? '\0' : 'P', tr);
		int subvarN = patternNames.size() - subvar0;
		int width = ((program.length() - start_pc - 1) << 3)
		  | (repeat ? MATCH_LREPEAT : MATCH_PAIR);
		if (width > 0xFFFF)
		  start_pc += insertInt(start_pc, program,
					(width >> 13) + MATCH_WIDE);
		program.setCharAt(start_pc, (char) width);

		int restLength = Translator.listLength(next);
		if (restLength == Integer.MIN_VALUE)
		  {
		    tr.syntaxError("cyclic pattern list");
		    return;
		  }

		if (repeat)
		  {
		    addInt(program, subvar0 << 3);
		    addInt(program, subvarN << 3);
		    if (next == LList.Empty)
		      {
			program.append((char) MATCH_NIL);
			return;
		      }
		    else
		      {
			// Map a signed int to an unsigned.
			restLength = restLength >= 0 ? restLength << 1
			  : ((-restLength) << 1) - 1;
			addInt(program, (restLength << 3) | MATCH_LENGTH);
		      }
		  }

		pattern = next;
		continue;
	      }
	    finally
	      {
		tr.popPositionOf(savePos);
	      }
	  }
	else if (pattern instanceof Symbol)
	  {
            ScopeExp current = tr.currentScope();
            ScopeExp scope1 = syntax == null ? current : syntax.getScope();
	    for (int j = literal_identifiers.length;  --j >= 0; )
	      {
                ScopeExp scope2;
                Object literal = literal_identifiers[j];
                if (literal instanceof SyntaxForm)
                  {
                    SyntaxForm syntax2 = (SyntaxForm) literal;
                    
                    literal = syntax2.getDatum();
                    scope2 = syntax2.getScope();
                  }
                else if (tr.currentMacroDefinition != null)
                  scope2 = tr.currentMacroDefinition.getCapturedScope();
                else
                  scope2 = current;
		if (literalIdentifierEq(pattern, scope1,
					literal, scope2))
		  {
		    int i = SyntaxTemplate.indexOf(literals, pattern);
		    if (i < 0)
		      {
			i = literals.size();
			literals.addElement(pattern);
		      }
		    addInt(program, (i << 3) | MATCH_EQUALS);
		    return;
		  }
	      }
            if (literalIdentifierEq(pattern, scope1, underscoreSymbol, null)) {
                program.append((char) MATCH_IGNORE);
                return;
            }
	    if (patternNames.contains(pattern))
	      tr.syntaxError("duplicated pattern variable " + pattern);
	    int i = patternNames.size();
	    patternNames.addElement(pattern);
	    boolean matchCar = context == 'P';
	    int n = (nesting << 1) + (matchCar ? 1 : 0);
	    patternScope.patternNesting.append((char) n);
            Declaration decl = patternScope.addDeclaration(pattern);
            decl.setInitValue(QuoteExp.undefined_exp);
            decl.setLocation(tr);
	    tr.push(decl);
	    addInt(program, (i << 3) | (matchCar ? MATCH_ANY_CAR : MATCH_ANY));
	    return;
	  }
	else if (pattern == LList.Empty)
	  {
	    program.append((char) MATCH_NIL);
	    return;
	  }
	else if (pattern instanceof FVector)
	  {
	    program.append((char) MATCH_VECTOR);
	    pattern = LList.makeList((FVector) pattern);
	    context = 'V';
	    continue;
	  }
	else
	  {
	    int i = SyntaxTemplate.indexOf(literals, pattern);
	    if (i < 0)
	      {
		i = literals.size();
		literals.addElement(pattern);
	      }
	    addInt(program, (i << 3) | MATCH_EQUALS);
	    return;
	  }
      }
  }

  private static void addInt (StringBuilder sbuf, int val)
  {
    if (val > 0xFFFF)
      addInt(sbuf, (val << 13) + MATCH_WIDE);
    sbuf.append((char) (val));
  }

  private static int insertInt (int offset, StringBuilder sbuf, int val)
  {
    if (val > 0xFFFF)
      offset += insertInt(offset, sbuf, (val << 13) + MATCH_WIDE);
    sbuf.insert(offset, (char) (val));
    return offset+1;
  }

  /** Match the <code>car</code> of a <code>Pair</code>.
   * This special case (instead of of just matching the <code>car</code>
   * directly), is so we can copy <code>PairWithPosition</code> line number
   * info into the output of a template. */
  boolean match_car (Pair p, Object[] vars, int start_vars,
		     int pc, SyntaxForm syntax)
  {
    int pc_start = pc;
    char ch;
    int value = (ch = program.charAt(pc++)) >> 3;
    while ((ch & 7) == MATCH_WIDE)
      value = (value << 13) | ((ch = program.charAt(pc++)) >> 3);
    if ((ch & 7) == MATCH_ANY_CAR)
      {
	if (syntax != null && ! (p.getCar() instanceof SyntaxForm))
	  p = Translator.makePair(p, SyntaxForms.fromDatum(p.getCar(), syntax), p.getCdr());
	vars[start_vars + value] = p;
	return true;
      }
    return match (p.getCar(), vars, start_vars, pc_start, syntax);
  }

  public boolean match (Object obj, Object[] vars, int start_vars,
			int pc, SyntaxForm syntax)
  {
    int value = 0;
    Pair p;
    for (;;)
      {
	while (obj instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) obj;
	    obj = syntax.getDatum();
	  }
	char ch = program.charAt(pc++);
	int opcode = ch & 7;
	value = (value << 13) | (ch >> 3);
	switch (opcode)
	  {
	  case MATCH_WIDE:
	    continue;
	  case MATCH_MISC:
	    if (ch == MATCH_NIL)
	      return obj == LList.Empty;
	    else if (ch == MATCH_VECTOR)
	      {
		if (! (obj instanceof FVector))
		  return false;
		return match(LList.makeList((FVector) obj),
			     vars, start_vars, pc, syntax);
	      }
	    else if (ch == MATCH_IGNORE)
	      return true;
	    else
	      throw new Error("unknown pattern opcode");
	  case MATCH_NIL:
	    return obj == LList.Empty;
	  case MATCH_LENGTH:
	    int npairs = value>>1;
	    Object o = obj;
	    for (int i = 0;;i++)
	      {
		while (o instanceof SyntaxForm)
		  o = ((SyntaxForm) o).getDatum();
		if (i == npairs)
		  {
		    if ((value&1) == 0 ? o != LList.Empty : o instanceof Pair)
		      return false;
		    break;
		  }
		else if (o instanceof Pair)
		  o = ((Pair) o).getCdr();
		else 
		  return false;
	      }
	    value = 0;
	    continue;
	  case MATCH_PAIR:
	    if (! (obj instanceof Pair))
	      return false;
	    p = (Pair) obj;
	    if (! match_car(p, vars, start_vars, pc, syntax))
	      return false;
	    pc += value;
	    value = 0;
	    obj = p.getCdr();
	    continue;	
	  case MATCH_LREPEAT:
	    int repeat_pc = pc;
	    pc += value;
	    int subvar0 = (ch = program.charAt(pc++)) >> 3;
	    while ((ch & 0x7) == MATCH_WIDE)
	      subvar0 = (subvar0 << 13) | ((ch = program.charAt(pc++)) >> 3);
	    subvar0 += start_vars;
	    int subvarN = program.charAt(pc++) >> 3;
	    while ((ch & 0x7) == MATCH_WIDE)
	      subvarN = (subvarN << 13) | ((ch = program.charAt(pc++)) >> 3);

	    ch = program.charAt(pc++);
	    boolean listRequired = true;
	    int pairsRequired;
	    if (ch == MATCH_NIL)
	      {
		pairsRequired = 0;
	      }
	    else
	      {
		value = ch >> 3;
		while ((ch & 0x7) == MATCH_WIDE)
		  value = (value << 13) | ((ch = program.charAt(pc++)) >> 3);
		if ((value & 1) != 0)
		  listRequired = false;
		pairsRequired = value >> 1;
	      }
	    int pairsValue = Translator.listLength(obj);
	    boolean listValue;

	    if (pairsValue >= 0)
	      listValue = true;
	    else
	      {
		listValue = false;
		pairsValue = -1-pairsValue;
	      }
	    if (pairsValue < pairsRequired || (listRequired && ! listValue))
	      return false;
	    int repeat_count = pairsValue - pairsRequired;
	    Object[][] arrays = new Object[subvarN][];

	    for (int j = 0;  j < subvarN;  j++)
	      arrays[j] = new Object[repeat_count];
	    for (int i = 0;  i < repeat_count;  i++)
	      {
		while (obj instanceof SyntaxForm)
		  {
		    syntax = (SyntaxForm) obj;
		    obj = syntax.getDatum();
		  }
		p = (Pair) obj;
		if (! match_car (p, vars, start_vars, repeat_pc, syntax))
		  return false;
		obj = p.getCdr();
		for (int j = 0;  j < subvarN;  j++)
		  arrays[j][i] = vars[subvar0+j];
	      }
	    for (int j = 0;  j < subvarN;  j++)
	      vars[subvar0+j] = arrays[j];
	    value = 0;
	    if (pairsRequired == 0 && listRequired)
	      return true;
	    continue;
	  case MATCH_EQUALS:
	    Object lit = literals[value];
            Translator tr = (Translator) Compilation.getCurrent();
            Syntax curSyntax = tr.getCurrentSyntax();
            ScopeExp sc1 = curSyntax instanceof Macro
                ? ((Macro) curSyntax).getCapturedScope()
                : null;
            ScopeExp sc2 = syntax == null ? tr.currentScope()
                : syntax.getScope();
            return literalIdentifierEq(lit, sc1, obj, sc2);
	  case MATCH_ANY:
	    if (syntax != null)
	      obj = SyntaxForms.fromDatum(obj, syntax);
	    vars[start_vars + value] = obj;
	    return true;
	  case MATCH_ANY_CAR: // Disallowed here.
	  default:
	    disassemble();
	    throw new Error("unrecognized pattern opcode @pc:"+pc);
	  }
      }
  }
  
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(program);
    out.writeObject(literals);
    out.writeInt(varCount);
    out.writeUTF(fileLine == null ? "" : fileLine);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    literals = (Object[]) in.readObject();
    program = (String)  in.readObject();
    varCount = in.readInt();
    String fline = in.readUTF();
    if (fline != null)
        fileLine = fline;
  }

    /** The compiler calls this method to implement syntax-case. */
    public static Object[] allocVars (int varCount, Object[] outer) {
        Object[] vars = new Object[varCount];
        if (outer != null) {
            int toCopy = outer.length;
            if (toCopy > varCount)
                toCopy = varCount;
            System.arraycopy(outer, 0, vars, 0, toCopy);
        }
        return vars;
    }

    public static boolean literalIdentifierEq(Object id1, ScopeExp sc1,
                                              Object id2, ScopeExp sc2) {
        if (id1 instanceof SyntaxForm) {
            SyntaxForm form1 = (SyntaxForm) id1;
            id1 = form1.getDatum();
            sc1 = form1.getScope();
        }
        if (id2 instanceof SyntaxForm) {
            SyntaxForm form2 = (SyntaxForm) id2;
            id2 = form2.getDatum();
            sc2 = form2.getScope();
        }
    if (id1 != id2 && (id1 == null || id2 == null || ! id1.equals(id2)))
      return false;
    if (sc1 == sc2)
      return true;
    Declaration d1 = null, d2 = null;
    // Ending the look before we get to ModuleExp isn't really right,
    // but it's a hassle dealing the global Environment.
    // FIXME when we re-do the library/globals implementation.
    while (sc1 != null && ! (sc1 instanceof ModuleExp))
      {
	d1 = sc1.lookup(id1);
	if (d1 != null)
	  break;
	sc1 = sc1.getOuter();
      }
    while (sc2 != null && ! (sc2 instanceof ModuleExp))
      {
	d2 = sc2.lookup(id2);
	if (d2 != null)
	  break;
	sc2 = sc2.getOuter();
      }
    return d1 == d2;
  }

  /** Parse the literals list in a syntax-rules or syntax-case. */
  public static Object[] getLiteralsList (Object list,
					  SyntaxForm syntax, Translator tr)
  {
    Object savePos = tr.pushPositionOf(list);
    int count = Translator.listLength(list);
    if (count < 0)
      {
	tr.error('e', "missing or malformed literals list");
	count = 0;
      }
    Object[] literals = new Object[count];
    for (int i = 0;  i < count;  i++)
      {
	while (list instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) list;
	    list = syntax.getDatum();
	  }
	Pair pair = (Pair) list;
	tr.pushPositionOf(pair);
	Object literal = pair.getCar();
	Object wrapped = SyntaxForms.fromDatumIfNeeded(literal, syntax);
        literal = Translator.stripSyntax(literal);
	if (! (literal instanceof Symbol))
          tr.error('e', "non-symbol '"+literal+"' in literals list");
	literals[i] = wrapped;
	list = pair.getCdr();
      }
    tr.popPositionOf(savePos);
    return literals;
  }

    public String toString() {
        StringBuilder sbuf = new StringBuilder("#<syntax-pattern");
        if (fileLine != null) {
            sbuf.append(' ');
            sbuf.append(fileLine);
        }
        sbuf.append('>');
        return sbuf.toString();
    }
}
