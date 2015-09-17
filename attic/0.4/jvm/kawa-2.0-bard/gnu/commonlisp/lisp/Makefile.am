## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_CLISP = primitives.lisp
# Required for Elisp
java_SCM = PrimOps.scm

PACKAGE_FNAME = gnu-commonlisp-lisp

EXTRA_DIST = $(java_CLISP) $(java_SCM)

EXTRA_GCJ_INPUTS = *.class
all: Makefile ../../../kawa/java-classes.stamp scm-classes.stamp clisp-classes.stamp $(@GCJ_COMPILED_SELECTED@)

../../../kawa/java-classes.stamp:
	cd ../../../kawa; $(MAKE)
