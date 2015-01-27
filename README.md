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

You can find a terse high-level description of the language in the 0.4 directory.

## Versions of Bard

- **0.2.x** An interpreter for a large subset of the language, used in a few commercial projects to implement rules systems and data compilers

- **0.3.x** An extension of the 0.2.x series of interpreters, which discarded certain domain-specific features in 0.2.x and replaced them with additional features of the core language. 0.3.x was marginally faster than 0.2.x, but both are fairly slow implementations.

- **0.4.x** The version that is presently under construction, Bard 0.4.x is a proper compiler to JVM bytecode that is built using  Per Bothner's [Kawa framework](http://www.gnu.org/software/kawa/index.html). 
The version of Bard that is currently under construction is both faster and more expressive than earlier versions of the language. It simplifies and consolidates the earlier language, but adds several powerful features without expanding the number of special cases a user has to learn about. The reference documentation is both more complete and shorter than earlier versions.

Bard 0.4.x has not yet been released, but has been built and tested on OS X, Windows, and Linux. Work on a 0.4 release is ongoing.

### Another Bard?

In a post to Google Code dated June 18th, 2011, Abe Pralle announced
that he was changing the name of his in-development programming
language from "Slag" to "Bard". I posted a comment to his blog
(http://bard-dev.blogspot.com/2012/10/welcome-to-bard-blog.html) to
let him know I was already using the name. The exchange was friendly,
and recently Abe graciously informed me that he was changing the name
of his language again, leaving the name "bard" to me. I want to thank
him for his kind consideration.

