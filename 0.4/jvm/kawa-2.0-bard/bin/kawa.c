/* A front-end using readline to "cook" input lines for Kawa.
 *
 * Copyright (C) 1999, 2004, 2007 Per Bothner
 * 
 * This front-end program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * Some code from Johnson & Troan: "Linux Application Development"
 * (Addison-Wesley, 1998) was used directly or for inspiration.
 *
 * Three different implementations are actually provided, see below.
 */

/* If use_telnet==PREFER_PTY, run the inferior using a pty. */
#define PREFER_PTY 0

/* If use_telnet==PREFER_TELNET_CLIENT, connect to the inferior using
   Telnet protocol.  This front-end is a telnet client. */
#define PREFER_TELNET_CLIENT 1

/* If use_telnet==PREFER_TELNET_CONNECT, connect to the inferior using
   Telnet protocol.  However, note the unusual set-up:  This front-end
   program starts up first as a server, starts up the inferior process,
   and then listens from connections from the inferior process.
   But once a connection has been made, the inferior acts like a
   telnet server, while this front-end acts like telnet client.
   This is the default and preferred implementation.  */
#define PREFER_TELNET_CONNECT 2

#ifndef SUPPORT_PTY
#define SUPPORT_PTY 0
#endif
#ifndef SUPPORT_TELNET
#define SUPPORT_TELNET 1
#endif
#ifndef SUPPORT_TELNET_CLIENT
#define SUPPORT_TELNET_CLIENT 0
#endif

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#if SUPPORT_TELNET
#include <arpa/telnet.h>
#endif
#include <signal.h>
#include <netdb.h>
#include <stdlib.h>
#include <errno.h>
#include <grp.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <readline/readline.h>
#include <readline/history.h>

#ifndef PREFERRED_PROTOCOL
#if SUPPORT_TELNET
#define PREFERRED_PROTOCOL PREFER_TELNET_CONNECT
#else
#define PREFERRED_PROTOCOL PREFER_PTY
#endif
#endif

extern char * get_classpath(const char *);

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

#ifdef GCJ_COMPILED
/* Make COMMAND relative to PROGNAME, which is main's argv[0]. */

char*
get_command (char *progname)
{
  char *command = COMMAND;
  char* rp = strrchr(progname, '/');
  if (rp != NULL)
    {
      int dirlen = rp - progname;
      char *tmp = malloc (dirlen + strlen(command) + 2);
      sprintf(tmp, "%.*s/%s", dirlen, progname, command);
      command = tmp;
    }
  return command;
}
#endif

#if defined(SUPPORT_PTY) && !defined(SUPPORT_TELNET)
static int use_telnet = 0;
#elif !defined(SUPPORT_PTY) && defined(SUPPORT_TELNET)
static int use_telnet = 2;
#else
static int use_telnet = PREFERRED_PROTOCOL;
#endif

#define APPLICATION_NAME "kawa"

static int in_from_inferior_fd;
static int out_to_inferior_fd;
static void set_edit_mode ();

#ifdef DEBUG
FILE *logfile = NULL;
#define DPRINT0(FMT) (fprintf(logfile, FMT), fflush(logfile))
#define DPRINT1(FMT, V1) (fprintf(logfile, FMT, V1), fflush(logfile))
#define DPRINT2(FMT, V1, V2) (fprintf(logfile, FMT, V1, V2), fflush(logfile))
#else
#define DPRINT0(FMT) ((void) 0) /* Do nothing */
#define DPRINT1(FMT, V1) ((void) 0) /* Do nothing */
#define DPRINT2(FMT, V1, V2) ((void) 0) /* Do nothing */
#endif

struct termios orig_term;

static pid_t child = -1;

/* This could also be used as a SIGCHLD handler.
   However, in that case for some reason we're not getting the
   correct exit code. */

static void
sig_child (int signo)
{
  int status;
  waitpid (child, &status, 0);
  DPRINT2("(Child process died.  status:%d, exited:%d.)\n", status, WIFEXITED (status));
  fflush(stderr);
  if (use_telnet)
    rl_deprep_terminal ();
  else
    tcsetattr(STDIN_FILENO, TCSANOW, &orig_term);
  exit (WIFEXITED (status) ? WEXITSTATUS (status) : 0);
}

#if SUPPORT_PTY

volatile int propagate_sigwinch = 0;

/* sigwinch_handler
 * propagate window size changes from input file descriptor to
 * master side of pty.
 */
void sigwinch_handler(int signal) { 
   propagate_sigwinch = 1;
}


/* get_slave_pty() returns an integer file descriptor.
 * If it returns < 0, an error has occurred.
 * Otherwise, it has returned the slave file descriptor.
 */

int get_slave_pty(char *name) { 
   struct group *gptr;
   gid_t gid;
   int slave = -1;

   /* chown/chmod the corresponding pty, if possible.
    * This will only work if the process has root permissions.
    * Alternatively, write and exec a small setuid program that
    * does just this.
    */
   if ((gptr = getgrnam("tty")) != 0) {
      gid = gptr->gr_gid;
   } else {
      /* if the tty group does not exist, don't change the
       * group on the slave pty, only the owner
       */
      gid = -1;
   }

   /* Note that we do not check for errors here.  If this is code
    * where these actions are critical, check for errors!
    */
   chown(name, getuid(), gid);
   /* This code only makes the slave read/writeable for the user.
    * If this is for an interactive shell that will want to
    * receive "write" and "wall" messages, OR S_IWGRP into the
    * second argument below.
    */
   chmod(name, S_IRUSR|S_IWUSR);

   /* open the corresponding slave pty */
   slave = open(name, O_RDWR);

   return (slave);
}
#endif

#if SUPPORT_TELNET
static int telnet_input_state = 0;

void
write_telnet_command2 (int code, int option)
{
  char buf[3];
  buf[0] = IAC;
  buf[1] = code;
  buf[2] = option;
  write (out_to_inferior_fd, buf, 3);
}

/* Used to store the the current state of negotiation of telnet options.
   For example, for option LINEMODE (34), (telnet_options_state[34] & 7)
   is the state of the option on this side, and
   ((telnet_options_state[34] >> 3) & 7) is the state on the other side.
   The 3 bits for each side can be any of OPTION_NO though OPTION_YES.
   The option is only enabled if the value is OPTION_YES.
   See RFC 1143. */

static char telnet_options_state[256];

/* The option is disabled, and no negotiating is in progress. */
#define OPTION_NO 0

/* We sent out DONT/WONT and are waiting for confirming WONT/DONT. */
#define OPTION_WANTNO 1

/* Like WANTNO, but we changed our mind. */
#define OPTION_WANTNO_OPPOSITE 2

/* We sent out DO/WILL and are waiting for confirming WILL/DO. */
#define OPTION_WANTYES 3

/* Like WANTYES, but we changed our mind. */
#define OPTION_WANTYES_OPPOSITE 4

/* The option is enabled, and no negotiating is in progress. */
#define OPTION_YES 5

/* Actually (try to) change the state for an option.
   Return false is we don't know how or don't want to.
   command is DO if we're enabling on this side;
   DONT if we're disabling on this side;
   WILL if we're enabling for the other side;
   WONT if we're disabling for the other side.

   You should not call this function directly.
   Instead, call request to send a request to the other side
   (but with DO/WILL and DONT/WONT switched).  Then, when
   confirmation comes back, it is handled by the handle method, which
   calls change.
   The telnet_options_state array may not have been updated yet.
*/

int
change (int command, int option)
{
  return 0;
}

/* Handle a request from the other side.
   Command is one of DO, DONT, WILL, WONT. */

static void
handle_telnet_option_request (int command, int option)
{
  /* True if the other side wants to change itself I.e. we got WILL/WONT);
     false if it wants us to change (i.e. we got DO/DONT). */
  int other_side = command < DO;

  /* True if DO or WILL; false if DONT or WONT. */
  int want_on = (command & 1) != 0;

  char state;
  
  DPRINT2 ("saw request (%d, %d)\n", command, option);

  option = option & 0xFF;
  state = telnet_options_state[option];
  if (other_side)
    state >>= 3;

  switch ((state >> 3) & 7)
      {
      case OPTION_YES:
	if (want_on)
	  return; /* Nothing to do. */
	/* Got a DONT or WONT.  Disable without arguing. */
	state = OPTION_NO;
	change(command, option);
	write_telnet_command2 (other_side ? DONT : WONT, option);
	break;
      case OPTION_NO:
	if (! want_on)
	  return; /* Nothing to do. */
	if (change (command, option))
	  {
	    state = OPTION_YES;
	    write_telnet_command2 (other_side ? DO : WILL, option);
	  }
	else
	  {
	    write_telnet_command2 (other_side ? DONT : WONT, option);
	  }
	break;
      case OPTION_WANTNO:
	state = OPTION_NO;
	break;
      case OPTION_WANTNO_OPPOSITE:
	/* if (goalState) Error:  DONT/WONT answered by WILL/DO.
	   Maybe some packets crossed in the mail.
	   Presumably the other side will take our original
	   request as de-conformation.  In any case: */
	state = OPTION_WANTYES;
	write_telnet_command2 (other_side ? DO : WILL, option);
	break;
      case OPTION_WANTYES:
	if (want_on)
	  {
	    state = OPTION_YES;
	    change (command, option);
	  }
	else
	  state = OPTION_NO; /* Declined. */
	break;
      case OPTION_WANTYES_OPPOSITE:
	if (want_on)
	  {
	    state = OPTION_WANTNO;
	    write_telnet_command2 (other_side ? DONT : WONT, option);
	  }
	else
	  {
	    state = OPTION_NO;
	  }
	break;
      }
    if (other_side)
      state = (telnet_options_state[option] & 0xC7) | (state << 3);
    else
      state = (telnet_options_state[option] & 0xF8) | state;
    telnet_options_state[option] = state;
}

static int
process_telnet_input (char *buffer, int length)
{
  char *in_ptr = buffer;
  char *out_ptr = buffer;

  while (in_ptr < buffer + length)
    {
      unsigned char ch = (unsigned char) *in_ptr++;
      switch (telnet_input_state)
	{
	case 0:
	  if (ch == IAC)
	    telnet_input_state = IAC;
	  else
	    *out_ptr++ = ch;
	  break;
	case IAC:
	  if (ch == IAC)
	    {
	      telnet_input_state = 0;
	      *out_ptr++ = ch;
	    }
	  else if (ch == WILL || ch == WONT || ch == DO || ch == DONT
		   || ch == SB)
	    {
	      telnet_input_state = ch;
	    }
	  else
	    {
	      telnet_input_state = 0;
	    }
	  break;
	case DO:
	case DONT:
	case WILL:
	case WONT:
	  handle_telnet_option_request (telnet_input_state, ch);
	  telnet_input_state = 0;
	  break;
#if 0
	case SB:
	case SB_IAC:
#endif
	}
    }
  return out_ptr - buffer;
}
#endif

static char buf[1024];
/* buf[0 .. buf_count-1] is the what has been emitted on the current line.
   It is used as the readline prompt. */
static int buf_count = 0;

int num_keys = 0;

/* True if readine input should be boldface.
   Default off because it isn't quite robust - sometimes
   the prompt becomes bold when it shouldn't. */   
int bold_input = 0;

static void
null_prep_terminal (int meta)
{
}

static void
null_deprep_terminal ()
{
  if (bold_input)
    {
      fprintf (rl_outstream, "\033[0m");
      fflush (rl_outstream);
    }
}

static int
pre_input_change_mode ()
{
  if (bold_input)
    {
      fprintf (rl_outstream, "\033[1m");
      fflush (rl_outstream);
    }
  return 0;
}

static void
line_handler (char *line)
{
  if (line == NULL)
    {
      if (use_telnet)
	{
#if SUPPORT_TELNET
	  static char buf[2];
	  buf[0] = IAC;
	  buf[1] = xEOF;
	  write (out_to_inferior_fd, buf, 2);
	  DPRINT0("Sent EOF.\n");
#endif
	}
      else
	{
	  char buf[1];
	  DPRINT0("saw eof!\n");
	  buf[0] = '\004'; /* ctrl/d */
	  write (out_to_inferior_fd, buf, 1);
	}
    }
  else
    {
      static char newline[] = "\r\n";
      /*  Send line to Kawa inferior: */
      write (out_to_inferior_fd, line, strlen (line));
      write (out_to_inferior_fd, newline, sizeof(newline)-1);
      if (*line)
	add_history (line);
      free (line);
    }
  rl_callback_handler_remove ();
  buf_count = 0;
  num_keys = 0;
}

int
main(int argc, char** argv)
{
  int in_from_tty_fd;
#if SUPPORT_TELNET
  char *hostname = "127.0.0.1";  /* localhost */
  int port = 5555;
  struct in_addr inaddr;
  struct hostent *host;
  int sock;
  struct sockaddr_in address;
#endif
  int child, i;
#if SUPPORT_PTY
  int master;
  char *name;
  struct sigaction act;
  struct winsize ws;
#endif
  struct termios t;
  int maxfd;
  fd_set in_set;
  static char empty_string[1] = "";
  char *prompt = empty_string;
  int out_argc = 0;
  int in_argc = 1;

#ifdef COMMAND_ARGS
  static char* command_args[] = { COMMAND_ARGS };
  char** out_argv = (char **)
    malloc (sizeof(command_args) + (5 + argc) * sizeof(char*));
  for (i = 0;  i < sizeof(command_args)/sizeof(char*);  i++)
    out_argv[out_argc++] = command_args[i];
#else
  char** out_argv = (char **) malloc ((7 + argc) * sizeof(char*));

#if defined(GCJ_COMPILED)
  out_argv[out_argc++] = get_command (argv[0]);
#else
  putenv (get_classpath(argv[0]));
  out_argv[out_argc++] = "java";
#endif

  /* Calculate and set the kawa.command.line property. */
  char command_prop[] = { "-Dkawa.command.line=" };
  int command_prop_len = sizeof(command_prop)-1;
  int sum_length = command_prop_len;
  for (i = 0;  i < argc;  i++)
    sum_length += strlen(argv[i])+1;
  char* command_line = malloc(sum_length);
  strcpy(command_line, command_prop);
  sum_length = command_prop_len;
  for (i = 0; ;)
    {
      char* arg = argv[i];
      int arglen = strlen(arg);
      strcpy(command_line+sum_length, arg);
      sum_length += arglen;
      if (++i == argc)
        break;
      command_line[sum_length++] = ' ';
    }
  out_argv[out_argc++] = command_line;

  /* Pass initial -D and -J flags to java. */
  for (; in_argc < argc; in_argc++)
    {
      char* arg = argv[in_argc];
      if (arg[0] != '-' || (arg[1] != 'D' && arg[1] != 'J'))
        break;
      if (arg[1] == 'J')
        arg = arg+2;
      out_argv[out_argc++] = arg;
    }

  out_argv[out_argc++] = "kawa.repl";
#endif

  if (! isatty(0) || ! isatty(1))
    {
      out_argv[out_argc++] = "--no-console";
      for (;  in_argc < argc;  in_argc++)
	out_argv[out_argc++] = argv[in_argc];
      out_argv[out_argc] = NULL;
      execvp(out_argv[0], out_argv);
      perror ("failed to exec " COMMAND);
      exit (-1);
    }

#ifdef DEBUG
  logfile = fopen("kawa-frontend.log", "w");
#endif

  set_edit_mode ();

  rl_readline_name = APPLICATION_NAME;

  if (! use_telnet)
    {
#if SUPPORT_PTY
      if ((master = OpenPTY(&name)) < 0)
	{
	  perror("kawa-frontend: could not open master pty");
	  exit(1);
	}

      DPRINT1("pty name: '%s'\n", name);

      /* set up SIGWINCH handler */
      act.sa_handler = sigwinch_handler;
      sigemptyset(&(act.sa_mask));
      act.sa_flags = 0;
      if (sigaction(SIGWINCH, &act, NULL) < 0)
	{
	  perror("kawa-frontend: could not handle SIGWINCH ");
	  exit(1);
	}

      if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) < 0)
	{
	  perror("kawa-frontend: could not get window size");
	}

      if ((child = fork()) < 0)
	{
	  perror("cannot fork");
	  exit(1);
	}

      if (child == 0)
	{ 
	  int slave;  /* file descriptor for slave pty */

	  /* We are in the child process */
	  close(master);

#ifdef TIOCSCTTY
	  if ((slave = get_slave_pty(name)) < 0)
	    {
	      perror("kawa-frontend: could not open slave pty");
	      exit(1);
	    }
#endif

	  /* We need to make this process a session group leader, because
	   * it is on a new PTY, and things like job control simply will
	   * not work correctly unless there is a session group leader
	   * and process group leader (which a session group leader
	   * automatically is). This also disassociates us from our old
	   * controlling tty. 
	   */
	  if (setsid() < 0)
	    {
	      perror("could not set session leader");
	    }

	  /* Tie us to our new controlling tty. */
#ifdef TIOCSCTTY
	  if (ioctl(slave, TIOCSCTTY, NULL))
	    {
	      perror("could not set new controlling tty");
	    }
#else
	  if ((slave = get_slave_pty(name)) < 0)
	    {
	      perror("kawa-frontend: could not open slave pty");
	      exit(1);
	    }
#endif

	  /* make slave pty be standard in, out, and error */
	  dup2(slave, STDIN_FILENO);
	  dup2(slave, STDOUT_FILENO);
	  dup2(slave, STDERR_FILENO);

	  /* at this point the slave pty should be standard input */
	  if (slave > 2)
	    {
	      close(slave);
	    }


	  /* Try to restore window size; failure isn't critical */
	  if (ioctl(STDOUT_FILENO, TIOCSWINSZ, &ws) < 0)
	    {
	      perror("could not restore window size");
	    }

	  /* now start the shell */
	  {
#if defined(GCJ_COMPILED) && ! defined(COMMAND_ARGS)
	    char *command_args[2];
	    char *progname
	    char *command = get_command (argv[0]);
	    command_args[0] = command;
	    command_args[1] = NULL;
	    execvp(command, command_args);
#else
	    static char* command_args[] = { COMMAND_ARGS, NULL };
	    execvp(COMMAND, command_args);
#endif
	  }

	  /* should never be reached */
	  exit(1);
	}

      /* Note that we only set termios settings for standard input;
       * the master side of a pty is NOT a tty.
       */
      tcgetattr(STDIN_FILENO, &orig_term);
      t = orig_term;
      t.c_lflag &= ~(ICANON | ISIG | ECHO | ECHOCTL | ECHOE | \
		     ECHOK | ECHOKE | ECHONL | ECHOPRT );
      t.c_iflag &= ~ICRNL;
      t.c_iflag |= IGNBRK;
      t.c_cc[VMIN] = 1;
      t.c_cc[VTIME] = 0;
      tcsetattr(STDIN_FILENO, TCSANOW, &t);
      in_from_inferior_fd = master;
      out_to_inferior_fd = master;
#else
      fprintf (stderr, "no pty support!\n");
      exit (1);
#endif
    }
  else /* use_telnet */
    {
#if SUPPORT_TELNET
      address.sin_family = AF_INET;

      if (use_telnet == 2)
	{
	  char port_buf[12];
	  socklen_t namelen;
	  int conn;
	  sock = socket (PF_INET, SOCK_STREAM, 0);
	  if (sock < 0)
	    {
	      perror ("cannot create socket");
	      exit (-1);
	    }
	  namelen = 1;
	  setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
		     (void*) &namelen, sizeof(int));
	  port = 0;
	  address.sin_port = htons(port);
	  memset (&address.sin_addr, 0, sizeof (address.sin_addr));
	  if (bind (sock, (struct sockaddr*) &address, sizeof (address)) != 0)
	    {
	      perror ("cannot bind socket");
	    }

	  namelen = sizeof (address);
	  if (getsockname (sock, (struct sockaddr*) &address, &namelen) != 0)
	    {
	      perror ("getsockname call failed");
	      exit(-1);
	    }
	  port = ntohs(address.sin_port);

	  if (listen (sock, 5) != 0)
	    {
	      perror ("listen on socket failed");
	      exit(-1);
	    }

	  out_argv[out_argc++] = "--connect";
	  sprintf (port_buf, "%d", port);
	  out_argv[out_argc++] = port_buf;
	  for (;  in_argc < argc;  in_argc++)
	    out_argv[out_argc++] = argv[in_argc];
	  out_argv[out_argc] = NULL;

	  child = fork();
	  if (child == 0)
	    {
	      execvp(out_argv[0], out_argv);
	      perror ("failed to exec " COMMAND);
	      exit (-1);
	    }
	  else if (child < 0)
	    {

	      perror ("failed to fork " COMMAND);
	      exit (-1);
	    }

	  conn = accept (sock, (struct sockaddr*) &address, &namelen);
	  if (conn < 0)
	    {
	      perror ("accept of socket failed");
	      exit(-1);
	    }
	  close (sock);
	  sock = conn;
 	}
#if SUPPORT_TELNET_CLIENT
      else
	{
	  if (inet_aton (hostname, &inaddr))
	    host = gethostbyaddr ((char*) &inaddr, sizeof(inaddr), AF_INET);
	  else
	    host = gethostbyname (hostname);
	  if (! host)
	    {
	      herror ("error looking up host");
	      exit (1);
	    }

	  sock = socket (PF_INET, SOCK_STREAM, 0);
	  if (sock < 0)
	    {
	      perror ("can't get socket");
	      exit (1);
	    }

	  address.sin_port = htons (port);
	  memcpy (&address.sin_addr,
		  host->h_addr_list[0], sizeof (address.sin_addr));

	  connect (sock, (struct sockaddr *) &address, sizeof(address));
	}
#endif
      in_from_inferior_fd = sock;
      out_to_inferior_fd = sock;

#else
      fprintf (stderr, "No telnet support!\n");
      exit (1);
#endif
    }

  rl_callback_handler_install (prompt, line_handler);
  if (use_telnet)
    {
      rl_prep_term_function = null_prep_terminal;
      rl_deprep_term_function = null_deprep_terminal;
      rl_pre_input_hook = pre_input_change_mode;
    }

  in_from_tty_fd = STDIN_FILENO;
  FD_ZERO (&in_set);
  maxfd = in_from_inferior_fd > in_from_tty_fd ? in_from_inferior_fd
    : in_from_tty_fd;
  for (;;)
    {
      int num;
      FD_SET (in_from_inferior_fd, &in_set);
      FD_SET (in_from_tty_fd, &in_set);

      num = select(maxfd+1, &in_set, NULL, NULL, NULL);

#if SUPPORT_PTY
      if (propagate_sigwinch)
	{
	  struct winsize ws;
	  if (ioctl (STDIN_FILENO, TIOCGWINSZ, &ws) >= 0)
	    {
	      ioctl (master, TIOCSWINSZ, &ws);
	    }
	  propagate_sigwinch = 0;
	  continue;
	}
#endif

      if (num <= 0)
	{
	  perror ("select");
	  exit (-1);
	}
      if (FD_ISSET (in_from_tty_fd, &in_set))
	{
	  int do_canon = 1;
	  DPRINT1("[tty avail num_keys:%d]\n", num_keys);

#if SUPPORT_PTY
	  if (! use_telnet)
	    {
	      struct termios term_master;
	      tcgetattr(master, &term_master);
	      DPRINT2 ("echo:%d, canon:%d\n",
		       (term_master.c_lflag & ECHO) != 0,
		       (term_master.c_lflag & ICANON) != 0);
	      do_canon = (term_master.c_lflag & ICANON) != 0;
	    }
#endif

	  if (do_canon == 0 && num_keys == 0)
	    {
	      char ch[10];
	      int count = read (STDIN_FILENO, ch, sizeof(ch));
	      write (out_to_inferior_fd, ch, count);
	    }
	  else
	    {
	      if (num_keys == 0)
		{
		  /* Re-install callback handler for new prompt. */
		  if (prompt != empty_string)
		    free (prompt);
		  prompt = malloc (buf_count + 1);
		  if (prompt == NULL)
		    prompt = empty_string;
		  else
		    {
		      memcpy (prompt, buf, buf_count);
		      prompt[buf_count] = '\0';
		      DPRINT1("New prompt '%s'\n", prompt);
		      if (buf_count > 0)
			write (1, "\r", 1);
		    }
		  rl_callback_handler_install (prompt, line_handler);
		}
	      num_keys++;
	      rl_callback_read_char ();
	    }
	}
      else /* output from Kawa inferior. */
	{
	  int i;
	  int count;
	  if (buf_count > (sizeof(buf) >> 2))
	    buf_count = 0;
	  count = read (in_from_inferior_fd, buf+buf_count,
			sizeof(buf) - buf_count);
	  if (count <= 0)
	    {
	      DPRINT0 ("(Connection closed by inferior)\n");
	      sig_child(-1);
	    }
	  DPRINT2("from inferior [%*.s]\n", count, buf+buf_count);
#if SUPPORT_TELNET
	  count = process_telnet_input (buf + buf_count, count);
#endif
	  write (1, buf + buf_count, count);
	  buf_count += count;
	  for (i = buf_count;  --i >= buf_count - count; )
	    {
	      if (buf[i] == '\n' || buf[i] == '\r')
		{
		  i++;
		  memmove (buf, buf+i, buf_count - i);
		  buf_count -= i;
		  break;
		}
	    }
	  DPRINT2("-> i: %d, buf_count: %d\n", i, buf_count);
	}
    }
}

static void set_edit_mode ()
{
  int vi = 0;
  char *shellopts;

  shellopts = getenv ("SHELLOPTS");
  while (shellopts != 0)
    {
      if (strncmp ("vi", shellopts, 2) == 0)
	{
	  vi = 1;
	  break;
	}
      shellopts = index (shellopts + 1, ':');
    }

  if (!vi)
    {
      if (getenv ("EDITOR") != 0)
	vi |= strcmp (getenv ("EDITOR"), "vi") == 0;
    }

  if (vi)
    rl_variable_bind ("editing-mode", "vi");
  else
    rl_variable_bind ("editing-mode", "emacs");
}
