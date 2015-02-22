package gnu.kawa.functions;
import gnu.math.*;
import gnu.text.*;
import java.text.Format;
import java.text.FieldPosition;
import java.text.ParseException;

/* Support for ~F, ~$, ~E, ~G. */

class LispRealFormat extends ReportFormat {
    char op;
    int arg1;
    int arg2;
    int arg3;
    int arg4;
    int arg5;
    int arg6;
    int arg7;
    boolean showPlus;
    boolean internalPad;

    /** Twice the number of args consumed; odd if any arg is PARAM_FROM_COUNT. */
    int argsUsed = -1;

    /** 'L': Common Lisp style; 'P' C/Java printf-style.
     * Used for fine points of printing 'g' style. */
    public char style = 'L';

    LispRealFormat() {
    }

    public Format resolve (Object[] args, int start) {
        if (argsUsed < 0) {
            argsUsed = (arg1 == LispFormat.PARAM_FROM_COUNT
                        || arg2 == LispFormat.PARAM_FROM_COUNT
                        || arg3 == LispFormat.PARAM_FROM_COUNT
                        || arg4 == LispFormat.PARAM_FROM_COUNT
                        || arg5 == LispFormat.PARAM_FROM_COUNT
                        || arg6 == LispFormat.PARAM_FROM_COUNT
                        || arg7 == LispFormat.PARAM_FROM_COUNT) ? 1 : 0;
            if (arg1 == LispFormat.PARAM_FROM_LIST) argsUsed += 2;
            if (arg2 == LispFormat.PARAM_FROM_LIST) argsUsed += 2;
            if (arg3 == LispFormat.PARAM_FROM_LIST) argsUsed += 2;
            if (arg4 == LispFormat.PARAM_FROM_LIST) argsUsed += 2;
            if (arg5 == LispFormat.PARAM_FROM_LIST) argsUsed += 2;
            if (arg6 == LispFormat.PARAM_FROM_LIST) argsUsed += 2;
            if (arg7 == LispFormat.PARAM_FROM_LIST) argsUsed += 2;
        }
        if (argsUsed > 0 && args == null)
            return this;
        if (op == '$') {
            FixedRealFormat mfmt = new FixedRealFormat();
            int decimals = getParam(this.arg1, 2, args, start);
            if (this.arg1 == LispFormat.PARAM_FROM_LIST)  start++;
            int digits = getParam(this.arg2, 1, args, start);
            if (this.arg2 == LispFormat.PARAM_FROM_LIST)  start++;
            int width = getParam(this.arg3, 0, args, start);
            if (this.arg3 == LispFormat.PARAM_FROM_LIST)  start++;
            char padChar = getParam(this.arg4, ' ', args, start);
            if (this.arg4 == LispFormat.PARAM_FROM_LIST)  start++;

            mfmt.setMaximumFractionDigits(decimals);
            mfmt.setMinimumIntegerDigits(digits);
            mfmt.width = width;
            mfmt.padChar = padChar;
            mfmt.internalPad = internalPad;
            mfmt.showPlus = showPlus;
            return mfmt;
        } else if (op == 'F' || op == 'f') {
            FixedRealFormat mfmt = new FixedRealFormat();
            int width = getParam(this.arg1, 0, args, start);
            if (this.arg1 == LispFormat.PARAM_FROM_LIST)  start++;
            int decimals = getParam(this.arg2, -1, args, start);
            if (this.arg2 == LispFormat.PARAM_FROM_LIST)  start++;
            int scale = getParam(this.arg3, 0, args, start);
            if (this.arg3 == LispFormat.PARAM_FROM_LIST)  start++;
            mfmt.overflowChar = getParam(this.arg4, '\0', args, start);
            if (this.arg4 == LispFormat.PARAM_FROM_LIST)  start++;
            char padChar = getParam(this.arg5, ' ', args, start);
            if (this.arg5 == LispFormat.PARAM_FROM_LIST)  start++;
            mfmt.setMaximumFractionDigits(decimals);
            mfmt.setMinimumIntegerDigits(0);
            mfmt.width = width;
            mfmt.scale = scale;
            mfmt.padChar = padChar;
            mfmt.internalPad = internalPad;
            mfmt.showPlus = showPlus;
            return mfmt;
        } else { // if (op == 'E' || op == 'G' || op == 'e' || op == 'g')
            ExponentialFormat efmt = new ExponentialFormat();
            efmt.exponentShowSign = true;
            efmt.width = getParam(this.arg1, 0, args, start);
            if (this.arg1 == LispFormat.PARAM_FROM_LIST)  start++;
            efmt.fracDigits = getParam(this.arg2, -1, args, start);
            if (this.arg2 == LispFormat.PARAM_FROM_LIST)  start++;
            efmt.expDigits = getParam(this.arg3, 0, args, start);
            if (this.arg3 == LispFormat.PARAM_FROM_LIST)  start++;
            efmt.intDigits = getParam(this.arg4, 1, args, start);
            if (this.arg4 == LispFormat.PARAM_FROM_LIST)  start++;
            efmt.overflowChar = getParam(this.arg5, '\0', args, start);
            if (this.arg5 == LispFormat.PARAM_FROM_LIST)  start++;
            efmt.padChar = getParam(this.arg6, ' ', args, start);
            if (this.arg6 == LispFormat.PARAM_FROM_LIST)  start++;
            efmt.exponentChar = getParam(this.arg7, 'E', args, start);
            if (this.arg7 == LispFormat.PARAM_FROM_LIST)  start++;
            efmt.general = op == 'G' || op == 'g';
            efmt.style = this.style;
            efmt.showPlus = showPlus;
            return efmt;
        }
    }

    public int format(Object[] args, int start, Appendable dst, FieldPosition fpos)
        throws java.io.IOException {
        StringBuffer sbuf = new StringBuffer(100);
        Format fmt = resolve(args, start);
        start += argsUsed >> 1;
        Number value = (Number) args[start++];
        fmt.format(value, sbuf, fpos);
        dst.append(sbuf);
        return start;
    }
}
