## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Buffer.java \
  BufferLocal.java \
  BufferReader.java \
  BufferWriter.java \
  CancelledException.java \
  Command.java \
  EKeymap.java \
  EFrame.java \
  EMenu.java \
  EToolkit.java \
  EWindow.java \
  InfProcessMode.java \
  InputStreamHandler.java \
  Marker.java \
  Mode.java \
  ProcessMode.java \
  ReplMode.java \
  SelfInsertCommand.java \
  TelnetMode.java \
  Signal.java

java_SCM = emacs.scm

EXTRA_DIST = emacs.scm package.html
GCJ_INPUTS = *emacs*.class
EXTRA_GCJ_DEPS = scm-classes.stamp
PACKAGE_FNAME = gnu-jemacs-buffer

all: Makefile java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
