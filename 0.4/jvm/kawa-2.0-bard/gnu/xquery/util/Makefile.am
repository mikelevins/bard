## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  ArithOp.java \
  Average.java \
  BooleanValue.java \
  CastableAs.java \
  CastAs.java \
  Compare.java \
  CompileMisc.java \
  Debug.java \
  DistinctValues.java \
  IntegerRange.java \
  ItemAt.java \
  MinMax.java \
  NamedCollator.java \
  NodeUtils.java \
  NumberValue.java \
  OrderedMap.java \
  OrderedTuples.java \
  QNameUtils.java \
  Reduce.java \
  RelativeStep.java \
  RelativeStepFilter.java \
  SequenceUtils.java \
  StringUtils.java \
  TimeUtils.java \
  ValuesFilter.java \
  XQException.java

java_SCM = Xutils.scm

PACKAGE_FNAME = gnu-xquery-util
SCM_COMPILE_FLAGS = --warn-undefined-variable --warn-unknown-member --warn-as-error

ALL = java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
all: Makefile $(ALL)

