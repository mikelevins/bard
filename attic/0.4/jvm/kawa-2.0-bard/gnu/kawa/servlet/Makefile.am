## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

if ENABLE_SERVLET
SERVLET_JSOURCES =\
  CGIServletWrapper.java \
  KawaPageServlet.java \
  KawaServlet.java \
  ReplServlet.java \
  ReplSession.java
SERVLET_SCM = servlets.scm
endif
if WITH_HTTPSERVER
HTTPSERVER_JSOURCES =\
  KawaHttpHandler.java
endif

java_sources=\
  HttpRequestContext.java \
  KawaAutoHandler.java \
  ServletPrinter.java \
  $(SERVLET_JSOURCES) $(HTTPSERVER_JSOURCES)

java_SCM = HTTP.scm $(SERVLET_SCM)

PACKAGE_FNAME = gnu-kawa-servlet

ALL = java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
all: Makefile $(ALL)
