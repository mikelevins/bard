## Process this with automake to create Makefile.in

java_sources = \
Access.java \
AnnotationDefaultAttr.java \
AnnotationEntry.java \
Attribute.java \
AttrContainer.java \
ObjectType.java ArrayType.java \
ClassType.java \
ConstantPool.java \
CpoolClass.java \
CpoolEntry.java \
CpoolInvokeDynamic.java \
CpoolValue1.java \
CpoolValue2.java \
CpoolNameAndType.java \
CpoolMethodHandle.java \
CpoolMethodType.java \
CpoolRef.java \
CpoolString.java \
CpoolUtf8.java \
EnclosingMethodAttr.java \
ExitableBlock.java \
Filter.java \
Location.java Field.java \
Label.java IfState.java TryState.java SwitchState.java \
Member.java \
Method.java \
CodeAttr.java ConstantValueAttr.java \
LineNumbersAttr.java LocalVarsAttr.java InnerClassesAttr.java \
MiscAttr.java \
ParameterizedType.java \
PrimType.java \
RuntimeAnnotationsAttr.java \
Scope.java \
SignatureAttr.java \
SourceDebugExtAttr.java \
SourceFileAttr.java \
SpecialObjectType.java \
StackMapTableAttr.java \
Type.java \
UninitializedType.java \
Variable.java \
VarEnumerator.java \
TypeVariable.java \
ZipArchive.java \
ZipLoader.java \
ArrayClassLoader.java \
ClassFileInput.java \
ClassTypeWriter.java \
ExceptionsAttr.java \
dump.java \
ListCodeSize.java

PACKAGE_FNAME = gnu-bytecode
GCJ_INPUTS =  $(java_Java)

EXTRA_DIST = package.html

include $(top_srcdir)/Make-rules

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
