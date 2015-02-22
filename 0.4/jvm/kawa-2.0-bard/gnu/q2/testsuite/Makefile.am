include $(top_srcdir)/Make-rules

KAWALIB = $(top_builddir)
KAWA = ../../../bin/kawa.sh

TEST_Q2_FILES = binary1.q2 comment1.q2 function1.q2 indent1.q2
EXTRA_DIST = $(TEST_Q2_FILES)

run-tests:
	CLASSPATH=$(top_builddir) $(JAVA) gnu.kawa.util.RunTestScript \
	  `for test in $(TEST_Q2_FILES); do echo $(srcdir)/$$test; done`

check: run-tests
