include $(top_srcdir)/Make-rules

KAWALIB = $(top_builddir)
KAWA = ../../../bin/kawa.sh
EXTRA_CLEAN = tmp* *.log testing.zip */*.class
mostlyclean-am: mostlyclean-generic mostlyclean-libtool
	rm -rf Mod1 Mod2

java_sources = TestMisc.java TestSuite.java RunXQTS.java

EXTRA_DIST = tab.xml.in maketab1.xql tab.html usemod.exp \
  multab.xql expectedmul.xml descend.xql expdescend.xml \
  latin1a.xql latin1a.expected latin1b.xml \
  auction.xml.in outline.xml.in usemod.xql mod1.xql mod2.xql mod2a.xql \
  bad-1.xql expected-1.errs XQTS-trace.exp xsltest1.xsl xsltest1.expected\
  bib.xml.in use-cases.xml book1.xml.in reviews.xml.in report1.xml.in \
  bids.xml.in users.xml.in items.xml.in format-users.xsl exp-format-users.html

all:

TestMisc.class:  TestMisc.java
	$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) $(JAVACFLAGS) $<

TestSuite.class:  TestSuite.java TestMisc.class
	$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) $(JAVACFLAGS) $<

RunXQTS.class:  RunXQTS.java
	$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) $(JAVACFLAGS) $<

XQTS_DIR = @XQTS_PATH@
XQTS_CATALOG = $(XQTS_DIR)/XQTSCatalog.xml
XQTS_JAVA_FLAGS = -Xmx120m

check-XQTS: RunXQTS.class
	CLASSPATH=$(KAWALIB) \
	  $(JAVA) $(XQTS_JAVA_FLAGS) gnu.xquery.testsuite.RunXQTS $(XQTS_DIR)
	@if diff -b $(srcdir)/XQTS-trace.exp XQuery-trace.log; \
	  then echo '# XQTS trace output matches'; \
	  else echo FAIL XQuery trace output mismatch; fi

check-misc: TestMisc.class
	CLASSPATH=$(KAWALIB) $(JAVA) -Dsrcdir=$(srcdir) gnu.xquery.testsuite.TestMisc

check-suite:  TestSuite.class
	CLASSPATH=$(KAWALIB) \
	  $(JAVA) gnu.xquery.testsuite.TestSuite $(srcdir)/use-cases.xml

maketab1.class:  maketab1.xql
	$(KAWA) --main --xquery -C $<

# Should not run $(JAVA) when built --with-gcj, as that runs
# interpreter gij.  Instead, should compile to native.
check-maketab1:  maketab1.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$(KAWALIB)" $(JAVA) maketab1 >tmp2
	@if diff -b $(srcdir)/tab.html tmp2; then echo '# maketab1 test passes'; else echo FAIL maketab1 test; fi

# FIXME - this does not do the correct thing
check-desc:
	$(KAWA) --xquery -e 'document("gnu/xquery/testsuite/bib.xml")//*[1]'

multab.class:  multab.xql
	$(KAWA) --main --xquery -C $<

check-multab:  multab.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$(KAWALIB)" $(JAVA) multab \
	  max=10 caption="Multiplication table " out:xml-indent=pretty >tmpmul
	@if diff -b $(srcdir)/expectedmul.xml tmpmul; then echo '# multab test passes'; else echo FAIL multab test; fi

descend.class:  descend.xql
	$(KAWA) --main --xquery -C $<

check-descend:  descend.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$(KAWALIB)" $(JAVA) descend >tmpdesc
	@if diff -b $(srcdir)/expdescend.xml tmpdesc; then echo '# descend test passes'; else echo FAIL descend test; fi

check-latin1a:
	$(KAWA) --xquery $(srcdir)/latin1a.xql >tmplatin1a
	@if diff -b $(srcdir)/latin1a.expected tmplatin1a; then echo '# latin1a test passes'; else echo FAIL latin1a test; fi

check-xsltest1:
	$(KAWA) --xslt -f $(srcdir)/xsltest1.xsl outline.xml >tmp-xsltest1.log
	@if diff -b $(srcdir)/xsltest1.expected tmp-xsltest1.log; \
	  then echo '# xsltest1 test passes'; \
	  else echo FAIL xsltest1 test; fi

check-format-users:
	$(KAWA) --xslt $(srcdir)/format-users.xsl users.xml \
	  |sed 's/^ *//'>tmp-format-users.html
	@if diff -b -B $(srcdir)/exp-format-users.html tmp-format-users.html; then echo '#  format-users test passes'; else echo FAIL format-users test; fi

./Mod1/mod1.class: mod1.xql
	$(CLASSPATH_ENV) $(KAWA) --module-static -d ../../.. -C $<
./Mod2/mod2.class: mod2.xql
	$(CLASSPATH_ENV) $(KAWA) -d ../../.. -C $<
./Mod2/mod2a.class: mod2a.xql
	$(CLASSPATH_ENV) $(KAWA) -d ../../.. -C $<
usemod.class: usemod.xql ./Mod1/mod1.class ./Mod2/mod2.class ./Mod2/mod2a.class
	$(CLASSPATH_ENV) $(KAWA) --main -C $<
check-mod: usemod.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$(KAWALIB)" $(JAVA) usemod >tmp-usemod
	@if diff -b $(srcdir)/usemod.exp tmp-usemod; \
	  then echo '# usemod test passes'; \
	  else echo FAIL usemod test; fi

bad-1.log: $(srcdir)/bad-1.xql
	-CLASSPATH=$(KAWALIB) $(KAWA) -C $< 2>&1 \
	| sed -e '1d' -e 's|^.*/bad-1.xql|bad-1.xql|' >bad-1.log
check-bad-1: bad-1.log
	@if diff -b $(srcdir)/expected-1.errs bad-1.log; \
	  then echo '# bad-1 test passes'; \
	  else echo FAIL bad-1 test; fi

check-without-XQTS:  check-misc check-suite check-maketab1 check-multab \
	  check-descend check-latin1a \
	  check-format-users check-mod check-bad-1 check-xsltest1

if WITH_XQTS
check: check-without-XQTS check-XQTS
else
check: check-without-XQTS
endif
