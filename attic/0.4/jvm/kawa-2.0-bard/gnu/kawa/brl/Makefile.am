## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  BRL.java \
  BRLRead.java \
  BRLReaderString.java

PACKAGE_FNAME = gnu-kawa-brl
GCJ_INPUTS =  $(java_Java)
java_SCM = progfun.scm
EXTRA_DIST = $(java_SCM)

all: Makefile java-classes.stamp scm-classes.stamp  $(@GCJ_COMPILED_SELECTED@)
