KAWALIB = ../../..
KAWA = ../../../bin/kawa.sh
DOTEST = $(KAWA) --clisp\
 -f "$(KAWALIB)/testsuite/testing.zip" $(OPTIONS)

SCRIPTS_TO_RUN = warnings.lisp
EXTRA_DIST = lang-test.lisp lambda.lisp $(SCRIPTS_TO_RUN)
CLEANFILES = *.log

all:

check: check-lang-test check-lambda-test check-run-test-scripts

check-lang-test:
	$(DOTEST) -f "$(srcdir)/lang-test.lisp" -e '(test-report)'

check-lambda-test:
	$(DOTEST) -f "$(srcdir)/lambda.lisp" -e '(test-report)'

check-run-test-scripts:
	CLASSPATH=$(top_builddir) $(JAVA) gnu.kawa.util.RunTestScript \
	  `for test in $(SCRIPTS_TO_RUN); do echo $(srcdir)/$$test; done`
