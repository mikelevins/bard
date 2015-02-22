## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

JAVAX_SCRIPT_EXTRA=\
  XQueryScriptEngineFactory.java
if WITH_JAVAX_SCRIPT
  JAVAX_SCRIPT_USED = $(JAVAX_SCRIPT_EXTRA)
endif

java_sources = \
  XQuery.java \
  XQParser.java \
  XQResolveNames.java \
  $(JAVAX_SCRIPT_USED)

PACKAGE_FNAME = gnu-xquery-lang
EXTRA_DIST = $(JAVAX_SCRIPT_EXTRA)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
