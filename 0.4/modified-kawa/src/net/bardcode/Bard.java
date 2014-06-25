package net.bardcode;
import gnu.expr.Language;
import gnu.kawa.lispexpr.*;
import gnu.lists.AbstractFormat;
import gnu.mapping.*;
import java.lang.Boolean;
import java.lang.reflect.Method;
import kawa.repl;
import kawa.Shell;
import kawa.ShellUpdater;
import kawa.standard.Scheme;
import net.bardcode.Print;

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

    public static Scheme getInstance()
    {
        if  (!(instance instanceof Bard)){new Bard();};
        return (Scheme)instance;    
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
        defAliasStFld("undefined", "net.bardcode.Structure","undefined");
        // built-in structures
        defAliasStFld("none", "net.bardcode.Structure","none");
        defAliasStFld("boolean", "net.bardcode.Structure","booleanStructure");
        defAliasStFld("integer", "net.bardcode.Structure","integerStructure");
        defAliasStFld("float", "net.bardcode.Structure","floatStructure");
        defAliasStFld("symbol", "net.bardcode.Structure","symbolStructure");
        defAliasStFld("keyword", "net.bardcode.Structure","keywordStructure");
        defAliasStFld("character", "net.bardcode.Structure","characterStructure");
        defAliasStFld("string", "net.bardcode.Structure","stringStructure");
        defAliasStFld("cons", "net.bardcode.Structure","consStructure");
        defAliasStFld("vector", "net.bardcode.Structure","vectorStructure");
        defAliasStFld("uri", "net.bardcode.Structure","uriStructure");
        defAliasStFld("method", "net.bardcode.Structure","methodStructure");

        defAliasStFld("box", "net.bardcode.Structure","boxStructure");
        defAliasStFld("class", "net.bardcode.Structure","classStructure");
        defAliasStFld("map", "net.bardcode.Structure","mapStructure");
        defAliasStFld("seq", "net.bardcode.Structure","seqStructure");
        defAliasStFld("function", "net.bardcode.Structure","functionStructure");
        defAliasStFld("protocol", "net.bardcode.Structure","protocolStructure");

        // not yet implemented structures
        //defAliasStFld("stream", "net.bardcode.Structure","streamStructure");
        //defAliasStFld("macro", "net.bardcode.Structure","macroStructure");
        //defAliasStFld("special-form", "net.bardcode.Structure","specialFormStructure");
        //defAliasStFld("wordvector", "net.bardcode.Structure","wordVectorStructure");

        // user structure constructors
        // sequence, record, synonym, and singleton can be used to
        // create new structures. The user can then use those
        // structures the same way as the built in structures.
        //defAliasStFld("sequence", "net.bardcode.Structure","sequenceStructure");
        //defAliasStFld("record", "net.bardcode.Structure","recordStructure");
        //defAliasStFld("synonym", "net.bardcode.Structure","synonymStructure");
        //defAliasStFld("singleton", "net.bardcode.Structure","singletonStructure");

        // structure getter
        defProcStFld("structure", "structures");

        // structure predicates
        //defProcStFld("singleton.value", "structures");
        defProcStFld("defined?", "structures");
        defProcStFld("undefined?", "structures");
        defProcStFld("something?", "structures");
        defProcStFld("nothing?", "structures");
        defProcStFld("boolean?", "structures");
        defProcStFld("character?", "structures");
        defProcStFld("float?", "structures");
        defProcStFld("integer?", "structures");
        defProcStFld("keyword?", "structures");
        defProcStFld("symbol?", "structures");
        defProcStFld("string?", "structures");
        defProcStFld("cons?", "structures");
        defProcStFld("vector?", "structures");
        defProcStFld("uri?", "structures");
        defProcStFld("method?", "structures");
        defProcStFld("box?", "structures");
        defProcStFld("class?", "structures");
        defProcStFld("map?", "structures");
        defProcStFld("seq?", "structures");
        defProcStFld("function?", "structures");
        defProcStFld("protocol?", "structures");

        // built-in special forms
        defSntxStFld("^", "kawa.standard.SchemeCompilation", "lambda");
        defSntxStFld("method", "kawa.standard.SchemeCompilation", "lambda");
        defSntxStFld("def", "kawa.lib.prim_syntax", "define");
        defProcStFld("default-prompter", "ports");
        // built-in methods and functions
        defProcStFld("write", "ports");
        defProcStFld("write-simple", "ports");
        defProcStFld("write-shared", "ports");
        defProcStFld("write-with-shared-structure", "ports");
        defProcStFld("display", "ports");
    }

    public String getName()
    {
        return "Bard";
    }

    public AbstractFormat getFormat(boolean readable)
    {
        return Print.bardWriteFormat;
    }

    /** The compiler inserts calls to this method for applications and applets. */
    public static void registerEnvironment()
    {
        Language.setDefaults(new Bard());
    }

    public ReadTable createReadTable ()
    {
        ReadTable rt = ReadTable.createInitial();
        ReaderDispatch rdispatch = ReaderDispatch.create(rt, false);
        rt.set('#', rdispatch);
        rt.setFinalColonIsKeyword(true);
        return rt;
    }

    public static void main(String args[]){
        Scheme bard = getInstance();
        repl theRepl = new repl(bard);
        Class[] emptyClasses = {};
        ShellUpdater.addFormat("bard","net.bardcode.Print","getBardFormat",emptyClasses);
        //ShellUpdater.showFormats();
        Shell.setDefaultFormat("bard");
        theRepl.main(args);
    }

}



