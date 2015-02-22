AC_INIT([kawa-bin],[2.0])
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE
AC_CANONICAL_HOST

AM_INIT_AUTOMAKE

AC_EXEEXT
AC_OBJEXT

AC_PROG_CC
AC_PROG_CXX

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

AM_CONDITIONAL(ENABLE_BRL, test "$enable_brl" = "yes")
AM_CONDITIONAL(ENABLE_JEMACS, test "$enable_jemacs" = "yes")
AM_CONDITIONAL(ENABLE_XML, test "$enable_xml" = "yes")
AM_CONDITIONAL(WITH_SAX2, test "$with_sax2" != "no")
AM_CONDITIONAL(WITH_SWING, test "$with_swing" != "no")
AM_CONDITIONAL(ENABLE_SERVLET,
  test "$enable_servlet" != "" -a "$enable_servlet" != "no")
AM_CONDITIONAL(WITH_SWT, test "$with_swt" = "yes")

AM_CONDITIONAL(ENABLE_KAWA_FRONTEND, test "$enable_kawa_frontend" = "yes")
AM_CONDITIONAL(WITH_LIBTOOL, test "$with_libtool" = "yes")
AM_CONDITIONAL(ON_WINDOWS, test "$CLASSPATH_SEPARATOR" = ";")

JAVA=${JAVA-java}

AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(JAVA)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
