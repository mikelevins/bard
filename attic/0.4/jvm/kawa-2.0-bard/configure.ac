qdnl Process this with autoconf to create configure
AC_INIT([kawa],[2.0]) dnl Also update bin/configure.ac and build.xml.
AC_CONFIG_SRCDIR([doc/kawa.texi])

AM_MAINTAINER_MODE
AC_CANONICAL_HOST

AM_INIT_AUTOMAKE

dnl AC_ARG_WITH(classpath,[  --with-classpath          Path to Java classes], classpath=$withval, classpath=$$CLASSPATH)
AC_ARG_ENABLE(kawa-frontend,
  [  --enable-kawa-frontend  Build "kawa" front-end program using readline],
  enable_kawa_frontend=$enableval, enable_kawa_frontend=no)

AC_ARG_WITH(libtool,
  AS_HELP_STRING(--with-libtool, Use libtool))

AC_ARG_ENABLE(xml,
  AS_HELP_STRING(--disable-xml,Don't build support for XML processing),
  enable_xml=$enableval, enable_xml=yes)

AC_ARG_WITH(servlet,
  AS_HELP_STRING(--with-servlet@<:@=servlet-api.jar@:>@,Build support for generating servlets))
WITH_SERVLET_ARG="${with_servlet}"

AC_ARG_WITH(android,
  AS_HELP_STRING(--with-android@<:@=android.jar@:>@,Build for the Android platform))

AC_ARG_WITH(javafx,
  AS_HELP_STRING(--with-javafx@<:@=jfxrt.jar@:>@,Enable support for JavaFX))
WITH_JAVAFX_ARG="${with_javafx}"

AC_ARG_WITH(XQTS,
  AS_HELP_STRING(--with-XQTS@<:@=XQTS-path@:>@,Support XQuery Test Suite))

AC_ARG_ENABLE(echo2,
  AS_HELP_STRING(--enable-echo2,Enable support for the Echo2 web toolkit))

AC_ARG_ENABLE(jemacs,
  AS_HELP_STRING(--enable-jemacs,Build JEmacs))

AC_ARG_ENABLE(xquery,
  AS_HELP_STRING(--enable-xquery,Build XQuery language support (Qexo) (default on)))

AC_ARG_ENABLE(brl,
  AS_HELP_STRING(--enable-brl,Build BRL emulation and KRL))

AC_ARG_WITH(httpserver,
  AS_HELP_STRING(--with-httpserver,Make use of JDK6's builtin http server))

AC_ARG_WITH(swing,
  AS_HELP_STRING(--with-swing,Assume Swing is available (default on)))

AC_ARG_WITH(swt,
  AS_HELP_STRING(--with-swt,Assume SWT is available))

AC_ARG_WITH(awt,
  AS_HELP_STRING(--with-awt,Assume AWT is available (default on)))

AC_ARG_WITH(sax2,
  AS_HELP_STRING(--with-sax2, Assume SAX2 is available (default on)))

AC_ARG_WITH(javax.script,
  AS_HELP_STRING(--with-javax.script, Use javax.script interfaces))

AC_ARG_WITH(java-source,
  AS_HELP_STRING(--with-java-source=NUM,Assume functionality of given Java/JDK version))

test -z "${with_java_source}" -a -n "${with_android}" \
  && with_java_source=android

with_java_source=`echo ${with_java_source} | tr A-Z a-z`
case "/${with_java_source}/" in
  /1/) JAVA_SOURCE=1 ;;
  /2/) JAVA_SOURCE=2 ;;
  /4/) JAVA_SOURCE=4 ;;
  /4x/) JAVA_SOURCE=4x ;;
  /5/) JAVA_SOURCE=5 ;;
  /6/) JAVA_SOURCE=6 ;;
  /6compat5/) JAVA_SOURCE=6compat5 ;;
  /7/) JAVA_SOURCE=7 ;;
  /8/) JAVA_SOURCE=8 ;;
  /1.1*/) JAVA_SOURCE=1 ;;
  /1.2*/) JAVA_SOURCE=2 ;;
  /1.3*/) JAVA_SOURCE=2 ;;
  /1.4*+jaxp1.3/) JAVA_SOURCE=4x ;;
  /1.4*/) JAVA_SOURCE=4 ;;
  /1.5*/) JAVA_SOURCE=5 ;;
  /1.6*/) JAVA_SOURCE=6 ;;
  /1.7*/) JAVA_SOURCE=7 ;;
  /1.8*/) JAVA_SOURCE=8 ;;
  /android*/)
        JAVA_SOURCE=android
        test -z "$with_swing" && with_swing="no"
        test -z "$with_awt" && with_awt="no"
        ;;
  //) JAVA_SOURCE=7 ;;
  /*/) AC_MSG_ERROR([Invalid version for --with-java-version.
  (Must be 1, 2, 5, 6, 7, 8, or a JDK version such as 1.5.)]) ;;
esac
AC_SUBST(JAVA_SOURCE)

if test -z "$with_sax2" ; then
  case "$JAVA_SOURCE" in
  1|2|3) with_sax2="no";;
  esac
fi

if test -z "$with_javax_script"; then
  case "$JAVA_SOURCE" in
  6compat5|6|7|8) with_javax_script="yes";;
  esac
fi

if test -z "$with_httpserver"; then
  case "$JAVA_SOURCE" in
  6compat5|6|7|8)
    test "$with_android" != "" -a "$with_android" != "no" \
      || with_httpserver="yes";;
  esac
fi

AC_PROG_INSTALL
AC_PROG_LN_S

TOP_BUILDDIR=`pwd`

JAVA=${JAVA-java}
JAVAC=${JAVAC-javac}
dnl needed to match _AM_DEPENDENCIES

JAVACFLAGS=${JAVACFLAGS--g}
case "$JAVA_SOURCE" in
  6compat5) JAVACFLAGS="${JAVACFLAGS} -target 1.5 -source 1.5" ;;
esac

pathsep="$PATH_SEPARATOR"
case ${host} in
  *-*-mingw*|*-*-cygwin*) CLASSPATH_SEPARATOR=";" ;;
  *) CLASSPATH_SEPARATOR="$PATH_SEPARATOR"
esac
filesep='/'

# Partly for compatibility (--enable/--disable-servlet was the old option);
# partly to allow --with-servlet=path-to-jar --disable-servlet.
# we essentially default enable-servlet to with-servlet. But we don't
# want to clutter up configure --help with --enable-servlet.
enable_servlet=${enable_servlet-${with_servlet}}

# Windows has a non-suitable find command.
AC_CACHE_CHECK([for Posix find command], [ac_cv_path_FIND],
            [AC_PATH_PROGS_FEATURE_CHECK([FIND], [find],
              [[findout=`$ac_path_FIND . -maxdepth 0 -type d -exec echo '{}' ';'`
                test "x$findout" = x"." \
                && ac_cv_path_FIND=$ac_path_FIND ac_path_FIND_found=:]],
              [AC_MSG_ERROR([could not find Posix find])])])
          AC_SUBST([FIND], [$ac_cv_path_FIND])

AM_CONDITIONAL(ENABLE_ECHO2, test "$enable_echo2" = "yes")
AM_CONDITIONAL(WITH_ANDROID,
  test "$with_android" != "" -a "$with_android" != "no")
AM_CONDITIONAL(ENABLE_BRL, test "$enable_brl" = "yes")
AM_CONDITIONAL(ENABLE_JEMACS, test "$enable_jemacs" = "yes")
AM_CONDITIONAL(ENABLE_XQUERY, test "$enable_xquery" != "no")
AM_CONDITIONAL(ENABLE_XML, test "$enable_xml" = "yes")
AM_CONDITIONAL(ENABLE_SERVLET,
  test "$enable_servlet" != "" -a "$enable_servlet" != "no")
AM_CONDITIONAL(WITH_SWING, test "$with_swing" != "no")
AM_CONDITIONAL(WITH_JAVAFX,
  test "$with_javafx" != "" -a "$with_javafx" != "no")
AM_CONDITIONAL(WITH_SWT, test "$with_swt" = "yes")
AM_CONDITIONAL(WITH_AWT, test "$with_awt" != "no")
AM_CONDITIONAL(WITH_SAX2, test "$with_sax2" != "no")
AM_CONDITIONAL(WITH_JAVAX_SCRIPT, test "$with_javax_script" = "yes")
AM_CONDITIONAL(WITH_HTTPSERVER, test "$with_httpserver" = "yes")
AM_CONDITIONAL(WITH_XQTS, test "$with_XQTS" != "" -a "$with_XQTS" != "no")
AM_CONDITIONAL(ENABLE_KAWA_FRONTEND, test "$enable_kawa_frontend" = "yes")
AM_CONDITIONAL(WITH_LIBTOOL, test "$with_libtool" = "yes")
AM_CONDITIONAL(ON_WINDOWS, test "$CLASSPATH_SEPARATOR" = ";")

JEMACS_VERSION=`echo $VERSION | sed  -e 's/^1/0/'`
BYTECODE_VERSION="$VERSION"

case ${with_servlet} in
  yes) ;;
  no) ;;
  "") ;;
  *) conf_classpath=${with_servlet}${CLASSPATH_SEPARATOR}$conf_classpath ;;
esac
case ${with_javafx} in
  yes) ;;
  no) ;;
  "") ;;
  *)
    if test -f "${with_javafx}" ; then true
    elif test -f "${with_javafx}/rt/lib/jfxrt.jar"
    then with_javafx="${with_javafx}/rt/lib/jfxrt.jar"
    elif test -f "${with_javafx}/lib/jfxrt.jar"
    then with_javafx="${with_javafx}/lib/jfxrt.jar"
    elif test -f "${with_javafx}/jre/lib/jfxrt.jar"
    then with_javafx="${with_javafx}/jre/lib/jfxrt.jar"
    fi
    conf_classpath=${with_javafx}${CLASSPATH_SEPARATOR}$conf_classpath ;;
esac
case ${with_android} in
  yes) ;;
  no) ;;
  "") ;;
  *) conf_classpath=${with_android}${CLASSPATH_SEPARATOR}$conf_classpath ;;
esac

case ${with_sax2} in
  yes) SAX2_PREPROCESS="+SAX2";;
  no) SAX2_PREPROCESS="-SAX2";;
  *) SAX2_PREPROCESS=""
esac

XQTS_PATH=${with_XQTS:-XQTS}
AC_SUBST(XQTS_PATH)
AC_SUBST(SAX2_PREPROCESS)
AC_SUBST(conf_classpath)
AC_SUBST(CLASSPATH_SEPARATOR)
AC_SUBST(pathsep)
AC_SUBST(filesep)
AC_SUBST(WITH_SERVLET_ARG)
AC_SUBST(WITH_JAVAFX_ARG)

AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAVACFLAGS)
#These don't do anything, because Make-rules can't contain autoconf
#substitutions.  Perhaps fix when new automake is out.
AC_SUBST(TOP_BUILDDIR)
AC_SUBST(PACKAGE)
AC_SUBST(VERSION)
AC_SUBST(JEMACS_VERSION)
AC_SUBST(BYTECODE_VERSION)

if test -z "$KAWA_SHELL"; then
  if test -f /bin/bash; then KAWA_SHELL=/bin/bash
  elif test -f /bin/ksh; then KAWA_SHELL=/bin/ksh
  else KAWA_SHELL=${CONFIG_SHELL-/bin/sh}
  fi
fi
AC_SUBST(KAWA_SHELL)

Make_rules="$srcdir/Make-rules"
AC_SUBST_FILE(Make_rules)
AC_CONFIG_LINKS(
  gnu/xquery/testsuite/outline.xml:gnu/xquery/testsuite/outline.xml.in
  gnu/xquery/testsuite/tab.xml:gnu/xquery/testsuite/tab.xml.in
  gnu/xquery/testsuite/bib.xml:gnu/xquery/testsuite/bib.xml.in
  gnu/xquery/testsuite/auction.xml:gnu/xquery/testsuite/auction.xml.in
  gnu/xquery/testsuite/book1.xml:gnu/xquery/testsuite/book1.xml.in
  gnu/xquery/testsuite/report1.xml:gnu/xquery/testsuite/report1.xml.in
  gnu/xquery/testsuite/reviews.xml:gnu/xquery/testsuite/reviews.xml.in
  gnu/xquery/testsuite/items.xml:gnu/xquery/testsuite/items.xml.in
  gnu/xquery/testsuite/bids.xml:gnu/xquery/testsuite/bids.xml.in
  gnu/xquery/testsuite/users.xml:gnu/xquery/testsuite/users.xml.in
  testsuite/scribble.html:testsuite/scribble.html.in)

AM_COND_IF([ENABLE_KAWA_FRONTEND],
        [AC_CONFIG_SUBDIRS([bin])])

AC_CONFIG_FILES([Makefile gnu/bytecode/Makefile gnu/math/Makefile
  gnu/Makefile bin/kawa.sh bin/qexo
  gnu/kawa/servlet/Makefile
  gnu/brl/Makefile gnu/kawa/brl/Makefile
  gnu/kawa/slib/Makefile
  gnu/commonlisp/lang/Makefile
  gnu/commonlisp/lisp/Makefile gnu/commonlisp/testsuite/Makefile
  gnu/jemacs/lang/Makefile gnu/jemacs/buffer/Makefile
  gnu/jemacs/lisp/Makefile gnu/jemacs/testsuite/Makefile
  gnu/jemacs/swing/Makefile gnu/jemacs/swt/Makefile
  kawa/Makefile kawa/lib/Makefile
  gnu/q2/lang/Makefile gnu/q2/testsuite/Makefile
  gnu/xquery/util/Makefile gnu/xquery/lang/Makefile
  gnu/xquery/testsuite/Makefile
  gnu/kawa/android/Makefile
  gnu/kawa/javafx/Makefile
  doc/Makefile testsuite/Makefile],[chmod +x bin/qexo bin/kawa.sh])
AC_OUTPUT
