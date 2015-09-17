## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Operator.java \
  Q2.java \
  Q2Apply.java \
  Q2Read.java \
  Q2Translator.java

PACKAGE_FNAME = gnu-q2-lang

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
