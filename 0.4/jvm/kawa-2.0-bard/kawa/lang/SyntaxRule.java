package kawa.lang;
import java.io.*;

/** A rule in an R5RS <code>syntax-template</code>.  A combination of
 * a <code>SyntaxPattern</code> and a <code>SyntaxTemplate</code>. */

public class SyntaxRule extends SyntaxTemplate implements Externalizable 
{
    SyntaxPattern pattern;

    public SyntaxRule() {
    }

    public SyntaxRule(SyntaxPattern pattern,
                      String pattern_nesting,
                      String template_program,
                      Object[] literal_values,
                      int max_nesting) {
        super(pattern_nesting, template_program, literal_values, max_nesting);
        this.pattern = pattern;
    }

    public SyntaxRule(SyntaxPattern pattern,
                      Object template,
                      SyntaxForm template_syntax,
                      Object ellipsis, Translator tr) {
        super(template, template_syntax, ellipsis, tr);
        this.pattern = pattern;
    }

    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(pattern);
        super.writeExternal(out);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        pattern = (SyntaxPattern) in.readObject();
        super.readExternal(in);
    }
}
