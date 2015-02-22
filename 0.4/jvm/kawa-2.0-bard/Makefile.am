# Process this with automake to create Makefile.in

FIXED_SUBDIRS = doc testsuite gnu/bytecode gnu/math \
  gnu \
  kawa gnu/kawa/slib gnu/kawa/servlet \
  gnu/commonlisp/lang gnu/commonlisp/lisp gnu/commonlisp/testsuite \
  gnu/q2/lang gnu/q2/testsuite
ANDROID_SUBDIRS = gnu/kawa/android
BRL_SUBDIRS = gnu/brl gnu/kawa/brl
JAVAFX_SUBDIRS = gnu/kawa/javafx
JEMACS_SUBDIRS = gnu/jemacs/lang gnu/jemacs/buffer gnu/jemacs/lisp \
  gnu/jemacs/testsuite gnu/jemacs/swing gnu/jemacs/swt
XQUERY_SUBDIRS = gnu/xquery/util gnu/xquery/lang gnu/xquery/testsuite

SUBDIRS = $(FIXED_SUBDIRS)
if ENABLE_JEMACS
  SUBDIRS += $(JEMACS_SUBDIRS)
endif
if ENABLE_XML
if ENABLE_BRL
SUBDIRS += $(BRL_SUBDIRS)
endif
if ENABLE_XQUERY
SUBDIRS += $(XQUERY_SUBDIRS)
endif
endif
if WITH_ANDROID
SUBDIRS += $(ANDROID_SUBDIRS)
endif
if WITH_JAVAFX
SUBDIRS += $(JAVAFX_SUBDIRS)
endif
if ENABLE_KAWA_FRONTEND
SUBDIRS += bin
endif

DIST_SUBDIRS = $(FIXED_SUBDIRS) \
  $(ANDROID_SUBDIRS) \
  $(BRL_SUBDIRS) \
  $(JAVAFX_SUBDIRS) \
  $(JEMACS_SUBDIRS) \
  $(XQUERY_SUBDIRS)

javadir=$(datadir)/java

SED = sed
CHMOD = chmod
JAR = jar
FIND = @FIND@
KAWAJAR = kawa-@VERSION@.jar
KAWALIB = $(datadir)/java/$(KAWAJAR)

META_ScriptEngineFactory = META-INF/services/javax.script.ScriptEngineFactory
if WITH_JAVAX_SCRIPT
META_INF_FILES = $(META_ScriptEngineFactory)
endif

classes.list: ./kawa-jar.stamp
	$(FIND) gnu kawa -name '*.class' -print >classes.list

classes-rt.list: ./kawa-jar.stamp $(srcdir)/classes-to-skip-for-runtime 
	$(FIND) gnu kawa -name '*.class' -print | grep -v -f $(srcdir)/classes-to-skip-for-runtime >classes-rt.list

kawa-@VERSION@.jar: ./kawa-jar.stamp $(META_INF_FILES) classes.list
	$(JAR) cmf $(srcdir)/jar-manifest $@ $(META_INF_FILES) @classes.list

kawart-@VERSION@.jar: ./kawa-jar.stamp classes-rt.list
	$(JAR) cf $@ @classes-rt.list

$(META_ScriptEngineFactory): Makefile
	echo 'kawa.standard.SchemeScriptEngineFactory #Scheme' >tmp.ScriptEngineFactory
if ENABLE_XQUERY
	echo 'gnu.xquery.lang.XQueryScriptEngineFactory #XQuery' >>tmp.ScriptEngineFactory
endif
	mkdir -p META-INF/services
	mv tmp.ScriptEngineFactory $(META_ScriptEngineFactory)

kawa-doc.tar.gz: kawa-doc-$(VERSION).tar.gz

kawa-doc-$(VERSION).tar.gz:  doc/kawa.texi
	cd doc; $(MAKE) ../kawa-doc-$(VERSION).tar.gz

dist-kawa-@VERSION@.jar: $(DIST_ARCHIVES)
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && tar xzf ../$(distdir).tar.gz \
	  && rm -f ./patch-source-list \
	  && ./kawa-@VERSION@/configure --with-java-source=7 \
	    --with-servlet=@WITH_SERVLET_ARG@ --with-javafx=@WITH_JAVAFX_ARG@ \
	  && CLASSPATH=@conf_classpath@.$(CLASSPATH_SEPARATOR)$$CLASSPATH $(MAKE) \
	  && mv kawa-@VERSION@.jar ../dist-kawa-@VERSION@.jar \
	  && cd .. && rm -rf tmpdir

jemacs-@JEMACS_VERSION@.jar:
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && $$srcdir/configure --with-swing \
	  && $(MAKE) \
	  && $(JAR) cmf $$srcdir/gnu/jemacs/jar-manifest \
	      ../jemacs-@JEMACS_VERSION@.jar \
	      `$(FIND) . -name '*.class' -print` \
	  && cd .. && rm -rf tmpdir

bytecode-@BYTECODE_VERSION@.jar: ./gnu/bytecode/java-classes.stamp
	$(JAR) cmf $(srcdir)/gnu/bytecode/jar-manifest \
	  bytecode-@BYTECODE_VERSION@.jar \
	  `$(FIND) gnu/bytecode -name '*.class' -print` 

kawa-@VERSION@-src.zip: kawa-@VERSION@.tar.gz
	mkdir zip-temp
	cd zip-temp ;\
	  tar xzf ../kawa-@VERSION@.tar.gz; \
	  zip -q -r ../kawa-@VERSION@-src.zip kawa-@VERSION@
	rm -rf zip-temp

kawa-compiled.zip: kawa-@VERSION@-compiled.zip
kawa.jar: kawa-@VERSION@.jar
kawa-src.zip: kawa-@VERSION@-src.zip
jemacs.jar: jemacs-@JEMACS_VERSION@.jar
bytecode.jar: bytecode-@BYTECODE_VERSION@.jar

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  config.guess config.sub install-sh ./mkinstalldirs \
  patch-source-list selected-java-source \
  build.xml build.properties \
  gnu/jemacs/jar-manifest gnu/jemacs/status.html \
  kawa/Version.java jar-manifest Make-rules classes-to-skip-for-runtime \
  bin/configure.ac bin/configure bin/Makefile.am bin/Makefile.in \
  bin/ChangeLog bin/kawa.bat.in bin/kawa.c bin/kawapath.c bin/cgi-servlet.c \
  bin/kawa.sh.in bin/qexo.in

MOSTLYCLEANFILES = tmp* *.o bin/*.o *.stamp classes.list classes-rt.list
CLEANFILES = *.jar bin/qexo kawa/Version.java bin/kawa.bat $(META_ScriptEngineFactory)
DISTCLEANFILES = selected-java-source bin/kawa.sh
MAINTAINERCLEANFILES = $(srcdir)/patch-source-list

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/api
JAVADOC_PACKAGES = \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text gnu.lists\
  gnu.xml gnu.kawa.reflect gnu.kawa.util gnu.kawa.lispexpr gnu.kawa.functions \
  gnu.kawa.servlet gnu.kawa.ant kawa kawa.lang kawa.standard kawa.lib \
  gnu.kawa.xml gnu.xquery.lang gnu.xquery.util gnu.brl gnu.kawa.brl \
  gnu.kawa.xslt gnu.kawa.sax gnu.commonlisp.lang gnu.q2.lang \
  gnu.kawa.models gnu.kawa.swingviews gnu.kawa.swtviews gnu.kawa.echo2 \
  gnu.jemacs.lang gnu.jemacs.buffer gnu.jemacs.swing gnu.jemacs.swt \
  gnu.kawa.android gnu.kawa.javafx

JAVADOC = javadoc
JAVADOC_FLAGS = -use
#JAVADOC_FLAGS = -J-Xmx60m

# NOTE:  To update javadoc:
#   make install-javadoc-html
# When satisfied:
#   make javadoc-cvs.log
# When satisfied:
#   cd ~/Kawa/api && cvs ci -m 'Re-run javadoc.'
javadoc-cvs.log:
	cd $(JAVADOC_DIR) \
	  && $(FIND) . -name CVS -prune -o -type f -exec echo rm '{}' ';'
	$(MAKE) install-javadoc-html >&LOG.JAVADOC
	(cd $(JAVADOC_DIR) && cvs -n update -d .>&/tmp/cvs-changes \
	  && (grep '^[?] ' /tmp/cvs-changes |sed -e 's|[?] ||') >/tmp/cvs-adds \
	  && (test '!' -s /tmp/cvs-adds || cvs add `cat /tmp/cvs-adds`) \
	  && (grep '^[U] ' /tmp/cvs-changes |sed -e 's|[U] ||') >/tmp/cvs-rems\
	  && (test '!' -s /tmp/cvs-rems || cvs remove `cat /tmp/cvs-rems`) \
	  && cvs update -d .) >cvs-update.log
	echo ExamineCheck ./cvs-update.log
	echo then: "(cd $(JAVADOC_DIR) && cvs ci -m 'Re-run javadoc.')".

ECLIPSE_PLUGINS_PATH = /opt/eclipse/plugins
JAVADOC_ANT_JARS = $(ECLIPSE_PLUGINS_PATH)/org.apache.ant_1.6.5/lib/ant.jar
JAVADOC_ECLIPSE_JARS = $(ECLIPSE_PLUGINS_PATH)/org.eclipse.jface_3.1.0.jar:$(ECLIPSE_PLUGINS_PATH)/org.eclipse.swt.gtk.linux.x86_3.1.0.jar
JAVADOC_EXTRA_PATH = $(JAVADOC_ECLIPSE_JARS):$(JAVADOC_ANT_JARS)

install-javadoc-html:
	-mkdir $(JAVADOC_DIR)
	-#rm `$(FIND) $(JAVADOC_DIR)/ -name CVS -prune -o '(' '!' -type d ')' -print`
	-rm `$(FIND) $(JAVADOC_DIR)/ -name '*.html'`
	CLASSPATH=$(JAVADOC_EXTRA_PATH)$(CLASSPATH_SEPARATOR)@conf_classpath@$$CLASSPATH \
	$(JAVADOC) $(JAVADOC_FLAGS) -sourcepath .:$(top_srcdir) -d $(JAVADOC_DIR) $(JAVADOC_PACKAGES)
	cp $(srcdir)/COPYING  $(JAVADOC_DIR)/COPYING

install-html: install-javadoc-html
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"

install-binSCRIPTS: $(bin_SCRIPTS)
	@$(NORMAL_INSTALL)
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	$(INSTALL_SCRIPT) bin/qexo $(DESTDIR)$(bindir)/qexo
if ! ENABLE_KAWA_FRONTEND
	$(SED) -e '/KAWALIB=/s|KAWALIB-\(.*\)}|KAWALIB-$(KAWALIB)}|' \
	  <bin/kawa.sh >tmp-kawa.sh
	$(CHMOD) +x tmp-kawa.sh
	$(INSTALL_SCRIPT) tmp-kawa.sh $(DESTDIR)$(bindir)/kawa
endif

install-data-am:  install-jar
install-jar:
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(javadir)
	$(INSTALL_DATA) kawa-@VERSION@.jar \
	  $(DESTDIR)$(javadir)/kawa-@VERSION@.jar
	cd $(DESTDIR)$(javadir) && rm -f kawa.jar \
	  && $(LN_S) kawa-@VERSION@.jar kawa.jar

install-bytecode-jar: bytecode-@BYTECODE_VERSION@.jar
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(javadir)
	$(INSTALL_DATA) bytecode-@BYTECODE_VERSION@.jar \
	  $(DESTDIR)$(javadir)/bytecode-@BYTECODE_VERSION@.jar
	cd $(DESTDIR)$(javadir) && rm -f bytecode.jar \
	  && $(LN_S) bytecode-@BYTECODE_VERSION@.jar bytecode.jar

uninstall-local: uninstall-jar uninstall-exec-bindir
uninstall-jar:
	rm -f $(DESTDIR)$(javadir)/kawa-@VERSION@.jar \
	  $(DESTDIR)$(javadir)/kawa.jar

uninstall-exec-bindir:
	rm -f $(DESTDIR)$(bindir)/kawa $(DESTDIR)$(bindir)/qexo

uninstall-bytecode-jar:
	rm -f $(DESTDIR)$(javadir)/bytecode-@BYTECODE_VERSION@.jar \
	  $(DESTDIR)$(javadir)/bytecode.jar

kawa/Version.java: $(srcdir)/kawa/Version.java.in $(srcdir)/configure.ac
	test -f $(srcdir)/.svn/entries \
	&& SVN_REVISION=`cd $(srcdir) && svnversion 2>/dev/null` \
	&& test -n "$$SVN_REVISION" \
	&& SVN_REVISION=" (revision $${SVN_REVISION})"; \
	sed -e 's|VERSION|@VERSION@'"$$SVN_REVISION|" <$(srcdir)/kawa/Version.java.in >$@

all-recursive:  kawa/Version.java selected-java-source
all: kawa-@VERSION@.jar kawart-@VERSION@.jar

JAVA_SOURCE = @JAVA_SOURCE@

if WITH_HTTPSERVER
PREPROCESS_HTTPSERVER = +use:com.sun.net.httpserver
else
PREPROCESS_HTTPSERVER = -use:com.sun.net.httpserver
endif
if ENABLE_XML
PREPROCESS_XML = +enable:XML
else
PREPROCESS_XML = -enable:XML
endif

selected-java-source: Makefile
	@java_source=`cat selected-java-source 2>/dev/null || echo none`; \
	if test "$$java_source" = "$(JAVA_SOURCE)"; \
	then echo current java-source is ok; \
	elif test "$(JAVA_SOURCE)" = 1; \
	then $(MAKE) select-java1; \
	elif test "$(JAVA_SOURCE)" = 2; \
	then $(MAKE) select-java2; \
	elif test "$(JAVA_SOURCE)" = 4; \
	then $(MAKE) select-java4; \
	elif test "$(JAVA_SOURCE)" = 4x; \
	then $(MAKE) select-java4x; \
	elif test "$(JAVA_SOURCE)" = 5; \
	then $(MAKE) select-java5; \
	elif test "$(JAVA_SOURCE)" = 6compat5; \
	then $(MAKE) select-java6compat5; \
	elif test "$(JAVA_SOURCE)" = 6; \
	then $(MAKE) select-java6; \
	elif test "$(JAVA_SOURCE)" = 7; \
	then $(MAKE) select-java7; \
	elif test "$(JAVA_SOURCE)" = 8; \
	then $(MAKE) select-java8; \
	elif test "$(JAVA_SOURCE)" = android; \
	then $(MAKE) select-android; \
	else echo Unrecognized JAVA_SOURCE: $(JAVA_SOURCE); exit 1; \
	fi

$(srcdir)/patch-source-list: $(top_srcdir)/configure
	(cd $(srcdir); \
	$(FIND) . -name '*.java' -exec grep -lE ' #endif ' '{}' ';') \
	  | sed -e 's|./||' | sort > $@

gnu/kawa/util/PreProcess.class: $(srcdir)/gnu/kawa/util/PreProcess.java
	$(JAVAC) -d . $(srcdir)/gnu/kawa/util/PreProcess.java

# JDK 1.1
select-java1: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java1'

# JDK 1.2+, GCJ-3.x
select-java2: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java2'

# JDK 1.4, GCJ-4.x, without JAXP
select-java4: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java4'

# JDK 1.4, GCJ-4.x with JAXP 1.3
select-java4x: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java4x'

# JDK 1.5 (default - matches SVN)
select-java5: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java5'

# JDK 1.5 with some Java6 features.
# Requires building with Java6, but runs on Java5 (without those features)
# or on Java6 (with those features).
select-java6compat5: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java6compat5'

# JDK 1.6
select-java6: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java6'

# JDK 1.7
select-java7: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java7'

# Java 8
select-java8: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%java8'

# Android
select-android: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(MAKE) do-preprocess SELECTOR='%android'

do-preprocess:
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$$CLASSPATH" $(JAVA) gnu.kawa.util.PreProcess \
	  $(SELECTOR) $(PREPROCESS_HTTPSERVER) $(PREPROCESS_XML) \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=$(JAVA_SOURCE)

set-selected-java-source:
	if test ! -e selected-java-source || \
	   test "$(JAVA_SOURCE)" != "`cat selected-java-source`"; \
	   then echo "$(JAVA_SOURCE)" >selected-java-source; \
	fi

if ON_WINDOWS
# kawa.sh is still needed for 'make check', even on Windows.
KAWASCRIPTS_KAWA = bin/kawa.bat bin/kawa.sh
else
KAWASCRIPTS_KAWA = bin/kawa.sh
endif
noinst_SCRIPTS = bin/kawa.sh
bin_SCRIPTS = bin/qexo $(KAWASCRIPTS_KAWA)

bin/kawa.bat: $(srcdir)/bin/kawa.bat.in
	sed -e 's|%DEFAULT_KAWA_VERSION%|$(VERSION)|' \
	    -e 's|%DEFAULT_EXTRA_PATH%|'`echo '$(conf_classpath)'| sed -e 's|[\]|\\\\|g' -e 's|\&|\\&|g'`'|' \
	  <$(srcdir)/bin/kawa.bat.in > bin/kawa.bat

distall:  distcheck dist-kawa-@VERSION@.jar kawa-doc-$(VERSION).tar.gz
# Consider also:
# kawa-@VERSION@-src.zip jemacs-@JEMACS_VERSION@.jar bytecode-@BYTECODE_VERSION@.jar
