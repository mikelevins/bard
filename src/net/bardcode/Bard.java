package net.bardcode;
import gnu.bytecode.ClassType;
import gnu.expr.Language;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import java.lang.Boolean;
import java.lang.reflect.Method;
import kawa.repl;
import kawa.Shell;
import kawa.standard.Scheme;

public class Bard extends Scheme {

    static Bard instance;

    public Bard ()
    {
        environ = bardEnvironment;
    }

    protected Bard (Environment env)
    {
        super(env);
    }

    protected static final SimpleEnvironment bardEnvironment
        = Environment.make("bard-environment", Scheme.kawaEnvironment);

    static
    {
        instance = new Bard();
        Environment.setCurrent(bardEnvironment);
        instance.initBard();
    }

    public static Bard getInstance()
    {
        if  (!(instance instanceof Bard)){new Bard();};
        return instance;    
    }

    private void initBard (){
        String bardMap[] = new String[3];
        bardMap[0] = "bard";
        bardMap[1]="bard";
        bardMap[2]="net.bardcode.Bard";
        registerLanguage(bardMap);
        gnu.mapping.Environment.setCurrent(environ);
        
        // base singletons
        defAliasStFld("true", "java.lang.Boolean","TRUE");
        defAliasStFld("false", "java.lang.Boolean","FALSE");
        defAliasStFld("nothing", "gnu.lists.LList","Empty");

        // built-in special forms
        defSntxStFld("^", "kawa.standard.SchemeCompilation", "lambda");
        defSntxStFld("method", "kawa.standard.SchemeCompilation", "lambda");

    }

    public String getName()
    {
        return "Bard";
    }

    /** The compiler inserts calls to this method for applications and applets. */
    public static void registerEnvironment()
    {
        Language.setDefaults(getInstance());
    }

    @Override
    public boolean keywordsAreSelfEvaluating() { return true; }

    @Override
    public ReadTable createReadTable ()
    {
        ReadTable tab = ReadTable.createInitial();
        ReaderDispatch dispatchTable = (ReaderDispatch) tab.lookup('#');
        ReaderDispatchSyntaxQuote sentry = new ReaderDispatchSyntaxQuote();
        dispatchTable.set('\'', sentry);
        dispatchTable.set('`', sentry);
        dispatchTable.set(',', sentry);
        tab.putReaderCtorFld("path", "gnu.kawa.lispexpr.LangObjType", "pathType");
        tab.putReaderCtorFld("filepath", "gnu.kawa.lispexpr.LangObjType", "filepathType");
        tab.putReaderCtorFld("URI", "gnu.kawa.lispexpr.LangObjType", "URIType");
        tab.putReaderCtor("symbol", ClassType.make("gnu.mapping.Symbol"));
        tab.putReaderCtor("namespace", ClassType.make("gnu.mapping.Namespace"));
        tab.putReaderCtorFld("duration", "kawa.lib.numbers", "duration");
        tab.postfixLookupOperator = ':';
        tab.setInitialColonIsKeyword(true);
        tab.setFinalColonIsKeyword(false);
        tab.extraFlags = LispReader.SCM_LEXPONENT_IS_BIGDECIMAL;
        tab.set('@', new ReaderQuote(LispLanguage.splice_sym,
                                     ':', LispLanguage.splice_colon_sym,
                                     ReadTable.NON_TERMINATING_MACRO));
        return tab;
    }

    @Override
    public String getPrimaryPrompt() { return "bard> "; }

    @Override
    public String getSecondaryPrompt() { return  "    > "; }

    public static void main(String args[]){
        Bard bard = getInstance();
        Bard.setCurrentLanguage(bard);
        ReadTable.setCurrent(bard.createReadTable());
        repl theRepl = new repl(bard);
        Class[] emptyClasses = {};
        theRepl.main(args);
    }

}

