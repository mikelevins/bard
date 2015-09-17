## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_ELISP = primitives.el alist.el simple.el keymap.el keydefs.el hanoi.el \
  rect.el editfns.el subr.el autoloads.el

PACKAGE_FNAME = gnu-jemacs-lisp
EXTRA_GCJ_COMPILED = elisp-classes.stamp
EXTRA_GCJ_INPUTS = *.class

EXTRA_DIST = $(java_ELISP)

all: Makefile elisp-classes.stamp $(@GCJ_COMPILED_SELECTED@)
