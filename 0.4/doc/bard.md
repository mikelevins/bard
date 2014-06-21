# bard 0.4

## basic ideas

Bard is made of protocols. Protocols are named collections of values and procedures, organized as dictionaries.

Protocols are named. The namespace for Protocols is unique, flat, and global. Bard protocol names use a reverse domain-name convention, like the one used in Java. Unlike Java's package names, Bard's protocol names have no necessary relationship to pathnames in a filesystem.

The bard.system protocol is built in, and contains Bard's foundation. The bard.user protocol is also built in, and is the default protocol in which new definitions are made.

Protocols are dictionaries whose keys are the names of variables. The variables refer to values, most of which are procedures.


## special forms

^
->
begin
cond
define
if
setter

## types
