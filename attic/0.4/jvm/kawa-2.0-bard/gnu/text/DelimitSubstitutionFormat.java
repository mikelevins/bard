package gnu.text;

import gnu.text.*;
import gnu.lists.UnescapedData;
import java.text.FieldPosition;
import java.util.List;

/** Output a value, surrounded by special delimiter characters.
 *
 * In the simple case, format each value using {@code base},
 * but surround the result with {@code markSubstitutionStart}
 * and {@code markSubstitutionStart}.  However, it the value is
 * and {@code UnescapedData} write it out as-is, with no marks.
 * If the value is a {@code List} (and not a {@code CharSequence}),
 * write each element of the list as above, and surround the entire
 * list with {@code markGroupStart} and {@code markGroupEnd}.
 *
 * This is currently used for enclosed expressions in shell/process
 * quasi-literals, to enable proper quotation/escaping.  It could be used
 * for other "mini-languages", such as sql statements.
 */

public class DelimitSubstitutionFormat extends ReportFormat {
    public static final char MARK_GROUP_START = 0xF200;
    public static final char MARK_GROUP_END = 0xF201;
    public static final char MARK_SUBSTITUTION_START = 0xF202;
    public static final char MARK_SUBSTITUTION_END = 0xF203;

    public char markGroupStart = MARK_GROUP_START;
    public char markGroupEnd = MARK_GROUP_END;
    public char markSubstitutionStart = MARK_SUBSTITUTION_START;
    public char markSubstitutionEnd = MARK_SUBSTITUTION_END;

    public DelimitSubstitutionFormat(ReportFormat base) {
        this.base = base;
    }

    public static DelimitSubstitutionFormat getInstance(ReportFormat base) {
        return new DelimitSubstitutionFormat(base);
    }

    ReportFormat base;

    public int format(Object[] args, int start, Appendable dst, FieldPosition fpos)
        throws java.io.IOException {
        if (start >= args.length) {
	    dst.append("#<missing format argument>");
	    return start;
        }
        Object arg = args[start];
        if (arg instanceof List && ! (arg instanceof CharSequence)) {
            dst.append(markGroupStart);
            Object[] tmp = new Object[1];
            for (Object a : (List) arg) {
                tmp[0] = a;
                format1(tmp, 0, dst, fpos);
            }
            dst.append(markGroupEnd);
        } else {
            format1(args, start, dst, fpos);
        }
        return start+1;
    }

    int format1(Object[] args, int start, Appendable dst, FieldPosition fpos)
        throws java.io.IOException {
        Object arg = args[start];
        if (arg instanceof UnescapedData) {
            dst.append(((UnescapedData) arg).getData());
            start = start + 1;
        } else {
            dst.append(markSubstitutionStart);
            start = base.format(args, start, dst, fpos);
            dst.append(markSubstitutionEnd);
        }
        return start;
    }
}

