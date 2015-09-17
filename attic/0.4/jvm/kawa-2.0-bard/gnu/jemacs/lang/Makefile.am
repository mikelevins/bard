## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  CatchableException.java \
  While.java \
  ELisp.java \
  AddOp.java \
  DivideOp.java \
  Print.java \
  NumberCompare.java \
  SaveExcursion.java \
  defcustom.java defgroup.java \
  lambda.java

java_SCM = MiscOps.scm NumberOps.scm
PACKAGE_FNAME = gnu-jemacs-lang
EXTRA_GCJ_INPUTS = MiscOps*.class NumberOps*.class

EXTRA_DIST = $(java_SCM) package.html
all: Makefile java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

