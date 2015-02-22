## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=

java_SCM = GroupObjectBuilder.scm MakeScene.scm defs.scm

GCJ_INPUTS = GroupObjectBuilder*.class MakeScene*.class defs*.class
EXTRA_GCJ_DEPS = scm-classes.stamp
PACKAGE_FNAME = gnu-kawa-javafx

all: Makefile scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
