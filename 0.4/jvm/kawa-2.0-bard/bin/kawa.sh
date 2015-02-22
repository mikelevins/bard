#!/bin/bash
thisfile=`which $0`
thisdir=`dirname $thisfile`
if [ "$#" -eq 0 ]
then
   command_line="$0"
else
   command_line="$0 $*"
fi
test -t 0 || no_console="--no-console"

# If configured with --enable-kawa-frontend then kawa.sh is
# only used for pre-install testing.  In that case
# we don't need to set KAWALIB, since kawapath.c looks for it in "..".
#  LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$thisdir
#  export LD_LIBRARY_PATH
#  exec $thisdir/kawa "$@"

# The next line is munged during installation:
KAWALIB=${KAWALIB-"`echo "$thisdir/../kawa-2.0.jar"`"}
CLASSPATH="${KAWALIB}:${CLASSPATH}"
export CLASSPATH

# This ugly duplication is so we only have to use arrays (which are
# non-Posix and non-portable) if there is a -D or -J option.
case "$1" in
    -D* | -J*)
        i=0
        for arg in "$@"; do
            case "$arg" in
                -D* | -J*)
                    jvm_args[i++]="$arg"
                    shift
                ;;
                *) break
                ;;
            esac
        done
        exec ${JAVA-"java"} -Dkawa.command.line="${command_line}" "${jvm_args[@]}" kawa.repl ${no_console} "$@"
        ;;
    *)
        exec ${JAVA-"java"} -Dkawa.command.line="${command_line}" kawa.repl ${no_console} "$@"
        ;;
esac
