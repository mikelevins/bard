## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

SUBDIRS = lib

# Use an environment variable for CLASSPATH so we pick up standard classes:
#CLASSPATH_ENV = CLASSPATH=..:$(srcdir)/..:$$CLASSPATH

java_WITH_AWT = \
  GuiConsole.java ReplPane.java ReplDocument.java \
  ReplPaneOutPort.java GuiInPort.java
JAVAX_SCRIPT_EXTRA=\
  standard/SchemeScriptEngineFactory.java

KAWA_LANG_JAVA_FILES = \
  lang/Pattern.java \
  lang/AnyPat.java \
  lang/EqualPat.java \
  lang/ListPat.java \
  lang/ListRepeatPat.java \
  lang/PairPat.java \
  lang/VarListPat.java \
  lang/Lambda.java \
  lang/Quote.java \
  lang/Translator.java \
  lang/Eval.java \
  lang/AutoloadProcedure.java \
  lang/Continuation.java \
  lang/CalledContinuation.java \
  lang/Syntax.java \
  lang/Macro.java \
  lang/SyntaxPattern.java \
  lang/SyntaxRule.java \
  lang/SyntaxRules.java \
  lang/PatternScope.java \
  lang/TemplateScope.java \
  lang/SyntaxTemplate.java \
  lang/AutoloadSyntax.java \
  lang/CompileFile.java \
  lang/SyntaxForm.java \
  lang/SyntaxForms.java \
  lang/Record.java \
  lang/GetFieldProc.java \
  lang/SetFieldProc.java \
  lang/RecordConstructor.java \
  lang/GenericError.java\
  lang/NamedException.java

KAWA_STANDARD_JAVA_FILES = \
  standard/set_b.java \
  standard/prim_method.java \
  standard/append.java \
  standard/fluid_let.java \
  standard/expt.java \
  standard/vector_append.java \
  standard/read_line.java \
  standard/char_ready_p.java \
  standard/load.java \
  standard/constant_fold.java \
  standard/define.java \
  standard/define_library.java \
  standard/define_syntax.java \
  standard/syntax_rules.java \
  standard/let_syntax.java \
  standard/syntax.java \
  standard/syntax_case.java \
  standard/require.java \
  standard/module_extends.java \
  standard/module_implements.java \
  standard/module_name.java \
  standard/module_compile_options.java \
  standard/with_compile_options.java \
  standard/module_static.java \
  standard/export.java \
  standard/define_unit.java \
  standard/syntax_error.java \
  standard/let.java \
  standard/begin.java \
  standard/call_with_values.java \
  standard/sleep.java \
  standard/try_catch.java \
  standard/make.java \
  standard/location.java \
  standard/define_alias.java \
  standard/define_variable.java \
  standard/define_autoload.java \
  standard/define_member_alias.java \
  standard/Scheme.java \
  standard/SchemeCompilation.java \
  standard/object.java \
  standard/define_class.java \
  standard/thisRef.java \
  standard/IfFeature.java \
  standard/Include.java \
  standard/ImportFromLibrary.java \
  standard/TracedProcedure.java

java_sources = \
  Shell.java \
  repl.java \
  Version.java \
  SourceMethodType.java \
  SourceType.java \
  TelnetInputStream.java \
  TelnetOutputStream.java \
  Telnet.java \
  TelnetRepl.java
java_sources += $(KAWA_LANG_JAVA_FILES)
java_sources += $(KAWA_STANDARD_JAVA_FILES)
if WITH_AWT
java_sources += $(java_WITH_AWT)
endif
if WITH_JAVAX_SCRIPT
java_sources += $(JAVAX_SCRIPT_EXTRA)
endif

EXTRA_DIST = Version.java.in $(java_WITH_AWT) $(JAVAX_SCRIPT_EXTRA)
EXTRA_CLEAN = lang/*.class standard/*.class

PACKAGE_FNAME = kawa-repl
GCJ_INPUTS =  $(java_Java)

# Force repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
