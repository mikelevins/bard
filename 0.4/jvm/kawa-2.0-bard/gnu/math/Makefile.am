## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Numeric.java \
  Quantity.java DQuantity.java CQuantity.java \
  Quaternion.java CQuaternion.java DQuaternion.java \
  Complex.java CComplex.java DComplex.java \
  NamedUnit.java BaseUnit.java Unit.java MulUnit.java Dimensions.java \
  RealNum.java DFloNum.java \
  RatNum.java \
  IntFraction.java \
  MPN.java \
  IntNum.java \
  BitOps.java \
  DateTime.java Duration.java \
  FixedRealFormat.java \
  ExponentialFormat.java

EXTRA_DIST = package.html

PACKAGE_FNAME = gnu-math
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
