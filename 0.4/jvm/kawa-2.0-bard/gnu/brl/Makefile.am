## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  random.java

PACKAGE_FNAME = gnu-brl
GCJ_INPUTS =  $(java_Java)

java_SCM = stringfun.scm
EXTRA_DIST = $(java_SCM)

all: Makefile java-classes.stamp scm-classes.stamp  $(@GCJ_COMPILED_SELECTED@)
