// Copyright (c) 1998, 2004, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** Maintains the state for generating a switch statement or expression.
 *
 * <h4>Simple example</h4>
 * <p>To translate:
 * <blockquote><pre>
 * switch (exps) {
 *   case 1: exp1; break;
 *   case 2: exp2; break;
 *   default: expd;
 * } </pre></blockquote>
 * you can do:
 * <blockquote><pre>
 * compile[exps]
 * SwitchState sw = code.startSwitch();
 * sw.addCase(1, code);
 * compile[exp1];
 * sw.exitSwitch(code);
 * sw.addCase(2, code);
 * compile[exp2];
 * sw.exitSwitch(code);
 * sw.addDefault(code);
 * compile[expd];
 * sw.finish(code);
 * </pre></blockquote>
 */

public class SwitchState {
    /** The smallest case value, so far. */
    int minValue;

    /** The largest case value, so far. */
    int maxValue;

    /** The number of cases (not including the default case). */
    int numCases;

    /** The case values, in numerical order (in values[0..numCases-1]). */
    int[] values;

    /** The case locations, in the same order as values. */
    Label[] labels;

    /** The location to jump to if none of the cases match. */
    Label defaultLabel;

    /** Location of the actual switch instruction. */
    Label switch_label;

    /** Start of the "cases".
     * This is used to store the type-state for each case. */
    Label cases_label;

    /** Code following the switch. */
    Label after_label;

    TryState outerTry;

    public int getMaxValue() { return maxValue; }

    public int getNumCases() { return numCases; }

    public SwitchState(CodeAttr code) {
        switch_label = new Label(code);
        cases_label = new Label(code);
        after_label = new Label(code);
        defaultLabel = new Label(code);
        outerTry = code.try_stack;

        numCases = 0;
    }

    /** Needs to be called after the switch value has been pushed.
     * I.e. in execution order, just before the actual switch instruction.
     * Called implicitly by {@link CodeAttr#startSwitch}.
     */
    public void switchValuePushed(CodeAttr code) {
        switch_label.setTypes(code);
        code.popType();  // pop switch value
        cases_label.setTypes(code);
        code.fixupChain(cases_label, switch_label);
    }

    /** Emit a new case, for the given value, whose label is here. */
    /** Add a new case.
     * @param value the case value to match against at run-time
     * @param code the CodeAttr of the Method we are generating code for
     * @return true on success;  false if value duplicates an existing value
     */
    public boolean addCase(int value, CodeAttr code){
        Label label = new Label(code);
        label.setTypes(cases_label);
        label.define(code);
        return insertCase(value, label, code);
    }

    /** Optimization of {@code addCase(value, code); emitGoto(label)}. */
    public boolean addCaseGoto(int value, CodeAttr code, Label label) {
        boolean ok = insertCase(value, label, code);
        label.setTypes(cases_label);
        code.setUnreachable();
        return ok;
    }

    public void addDefault(CodeAttr code) {
        if (defaultLabel.defined()) throw new Error();
        if (outerTry != code.try_stack)
            defaultLabel.setTypes(code);
        defaultLabel.setTypes(cases_label);
        defaultLabel.define(code);
    }

    /** Internal routine to add a new case.
     * @param value the case value to match against at run-time
     * @param label the location to go to if the value matches
     * @param code the CodeAttr of the Method we are generating code for
     * @return true on success;  false if value duplicates an existing value
     */
    public boolean insertCase(int value, Label label, CodeAttr code) {
        if (values == null) {
            values = new int[10];
            labels = new Label[10];
            numCases = 1;
            minValue = maxValue = value;
            values[0] = value;
            labels[0] = label;
            return true;
        }
        int[] old_values = values;
        Label[] old_labels = labels;
        if (numCases >= values.length) {
            values = new int[2 * numCases];
            labels = new Label[2 * numCases];
        }
        int copyBefore;
        if (value < minValue) {
            copyBefore = 0;
            minValue = value;
        } else if (value > maxValue) {
            copyBefore = numCases;
            maxValue = value;
        } else {
            // Binary search.
            int low = 0;
            int hi = numCases - 1;
            copyBefore = 0;
            while (low <= hi) {
                copyBefore = (low + hi) >>> 1;
                if (old_values[copyBefore] >= value)
                    hi = copyBefore - 1;
                else
                    low = ++ copyBefore;
            }

            if (value == old_values[copyBefore])
                return false;
        }
        int copyAfter = numCases - copyBefore;
        System.arraycopy(old_values, copyBefore, values, copyBefore+1, copyAfter);
        System.arraycopy(old_values, 0, values, 0, copyBefore);
        values[copyBefore] = value;
        System.arraycopy(old_labels, copyBefore, labels, copyBefore+1, copyAfter);
        System.arraycopy(old_labels, 0, labels, 0, copyBefore);
        labels[copyBefore] = label;
        numCases++;
        return true;
    }

    /** Break/exit from this switch.
     * Doesn't allow exiting through a try - if you need that,
     * use an {@link ExitableBlock}.
     */
    public void exitSwitch(CodeAttr code) {
        if (code.reachableHere()) {
            if (outerTry != code.try_stack)
                throw new Error("exitSwitch cannot exit through a try");
            code.emitGoto(after_label);
        }
    }
  
    /** Handle the end of the switch statement.
     * Assume the case value is on the stack; go to the matching case label. */
    public void finish (CodeAttr code) {
        if (code.reachableHere())
            exitSwitch(code);
        if (!defaultLabel.defined()) {
            defaultLabel.define(code);
            ClassType ex = ClassType.make("java.lang.RuntimeException");
            code.emitNew(ex);
            code.emitDup(ex);
            code.emitPushString("bad case value!");
            Type[] args = { Type.string_type };
            Method con = ex.addMethod("<init>", Access.PUBLIC,
                                      args, Type.voidType);
            code.emitInvokeSpecial(con);
            code.emitThrow();
        }
        Label end_label = new Label(code);

        // When numCases > 1 we are going to generate code
        // that will be moved before the already generated 
        // code of the case clauses. We need to set the 
        // stack map to reflect the correct state.
        code.setTypes((numCases <= 1) ? switch_label : cases_label);

        code.fixupChain(switch_label, end_label);
        if (numCases <= 1) {
            if (numCases == 1) {
                if (minValue == 0)
                    code.emitIfIntEqZero();
                else {
                    code.emitPushInt(minValue);
                    code.emitIfEq();
                }
                code.emitGoto(labels[0]);
                code.emitElse();
                code.emitGoto(defaultLabel);
                code.emitFi();
            } else {
                code.emitPop(1);
                code.emitGoto(defaultLabel);
            }
        } else {
            long rangeDim = (long)maxValue - (long)minValue;
            if (2 * numCases >= rangeDim) {
                int size = (int) (13 + 4 * (rangeDim + 1));
                code.reserve(size);
                code.fixupAdd(CodeAttr.FIXUP_SWITCH, null);
                code.put1(170);  // tableswitch
                code.fixupAdd(CodeAttr.FIXUP_CASE, defaultLabel);
                code.PC += 4;
                code.put4(minValue);
                code.put4(maxValue);
                int index = 0;
                // convoluted code in case maxValue==Integer.MAX_VALUE
                for (int i = minValue; ; i++) {
                    Label lab = values[index] == i ? labels[index++] : defaultLabel;
                    code.fixupAdd(CodeAttr.FIXUP_CASE, lab);
                    code.PC += 4;
                    if (i == maxValue) break;
                }
            } else {
                code.reserve(9 + 8 * numCases);
                code.fixupAdd(CodeAttr.FIXUP_SWITCH, null);
                code.put1(171);  // lookupswitch
                code.fixupAdd(CodeAttr.FIXUP_CASE, defaultLabel);
                code.PC += 4; // make room for defaultLabel
                code.put4(numCases);
                for (int index = 0;  index < numCases;  index++) {
                    code.put4(values[index]);
                    code.fixupAdd(CodeAttr.FIXUP_CASE, labels[index]);
                    code.PC += 4;
                }
            }
        }
        code.fixupChain(end_label, cases_label);
        code.setUnreachable();
        if (after_label.localTypes != null)
            after_label.define(code);
    }
}
