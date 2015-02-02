# Bard 
A programming language

by mikel evins

Bard is a small, high-level, general-purpose programming language. It
is interactive, dynamic, and impurely functional. It has a unique
object system influenced by, but different from, CLOS.

Bard is a dialect of Lisp, because Lisp is the family of languages
that offer me the most pleasure in programming. Bard is designed
primarily to make me happy--to make my work more pleasant. If someone
else likes it or finds it interesting that's a good thing, but it is
not my primary goal.

## Versions of Bard

- **0.2.x** An interpreter for a large subset of the language, used in a few commercial projects to implement rules systems and data compilers

- **0.3.x** An extension of the 0.2.x series of interpreters, which discarded certain domain-specific features in 0.2.x and replaced them with additional features of the core language. 0.3.x was marginally faster than 0.2.x, but both are fairly slow implementations.

- **0.4.x** The version that is presently under construction. There is
  a proper Bard 0.4.x compiler to JVM bytecode, built using Per
  Bothner's [Kawa
  framework](http://www.gnu.org/software/kawa/index.html), but I've
  temporarily paused that effort in order to work some more on a
  high-level VM for Bard. I went through another spasm of
  simplification that resulted in a still smaller language than
  before, and I want to work out its details in a simple virtual
  machine. The source code for that machine is what's currently in the
  0.4 subdirectory of the repo. I expect to add a new version of the
  Bard manual and revamp the JVM-based compiler to comply with the
  design of the Bard VM once it's far enough along.

Why the separate VM for Bard? Well, I've been interested in VMs for a
long time. I had a few ideas I wanted to try, and the VM serves as a
good arena for those experiments. Besides that, the VM is written in
Gambit-C Scheme, which makes it very portable--portable to places that
are not so convenient for the JVM. Furthermore, Gambit's creator, Marc
Feeley, has been working on a pretty interesting Javascript back-end
for Gambit, which raises the prospect of running the Bard VM in the
browser. That's very interesting to me, too.

### Another Bard?

In a post to Google Code dated June 18th, 2011, Abe Pralle announced
that he was changing the name of his in-development programming
language from "Slag" to "Bard". I posted a comment to his blog
(http://bard-dev.blogspot.com/2012/10/welcome-to-bard-blog.html) to
let him know I was already using the name. The exchange was friendly,
and recently Abe graciously informed me that he was changing the name
of his language again, leaving the name "bard" to me. I want to thank
him for his kind consideration.



