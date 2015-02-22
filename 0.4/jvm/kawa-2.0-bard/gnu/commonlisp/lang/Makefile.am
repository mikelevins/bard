## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Lisp2.java \
  Lisp2Compilation.java \
  defvar.java \
  defun.java \
  function.java \
  OrdinaryLambda.java \
  prog1.java \
  setq.java \
  Symbols.java \
  UnwindProtect.java \
  CommonLisp.java

PACKAGE_FNAME = gnu-commonlisp-lang

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
