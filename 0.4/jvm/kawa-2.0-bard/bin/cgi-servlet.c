/* A front-end to allow a servlet to be run as a CGI script.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char **environ;
extern char* get_classpath(const char *);
char *progname;

#ifndef JAVA
#define JAVA "java"
#endif

#ifndef COMMAND
#ifdef GCJ_COMPILED
#define COMMAND "kawa-bin"
#else
#define COMMAND JAVA
#endif
#endif

int
main(int argc, char** argv)
{
  char* progname = argv[0];
  char *basename = progname;
  char *p;
  char**envp;
  int env_size = 0, env_count = 0;
  char** nargv, **nargp;

  for (p = progname;  *p;  p++)
    {
      if (*p == '/')
	basename = p + 1;
    }
  /*printf("basename: %s\n", basename);*/

#ifndef GCJ_COMPILED
  p = getenv("CLASSPATH");
  if (p == NULL)
    {
      if (basename == progname)
	putenv("CLASSPATH=.");
      else
	{
	  p = malloc (basename-progname + 50);
	  sprintf(p, "CLASSPATH=%.*s", basename-progname-1, progname);
	  putenv(p);
	}
    }
  putenv (get_classpath(argv[0]));
#endif

  for (envp = environ; *envp != NULL; envp++)
    {
      env_count++;
      env_size += strlen (*envp);
    }
  nargv = (char**) malloc ((env_count + 5) * sizeof(char*));

  nargp = nargv;
  /**nargp++ = "/bin/echo";*/
  *nargp++ = COMMAND;
  for (envp = environ; *envp != NULL; envp++)
    {
      *nargp = (char*) malloc(strlen(*envp) + 3);
      sprintf(*nargp, "-D%s", *envp);
      nargp++;
    }
  *nargp++ = "gnu.kawa.servlet.CGIServletWrapper";
  *nargp++ = basename;
  *nargp++ = NULL;
  execvp(nargv[0], nargv);
}
