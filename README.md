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

### 0.2

The Bard 0.2 interpreter was a partial implementation of the language that served as a useful scripting tool and rules language during development of a family of educational games funded by the Office of Naval Research. It includes features specific to the game framework from that project. It's an AST-walking interpreter implemented in Gambit Scheme. It's not particularly fast, but it performed quite well enough for the purposes we used it for.

### 0.3

Bard 0.3 is an evolution of the 0.2 interpreter. It removes features specific to the game framework of the 0.2 version, and implements more of the Bard language design as it existed during the development of 0.3. It's roughly as fast as the 0.2 interpreter, which is to say fast enough for casual use and experimentation, but not very fast.

### 0.4

Bard 0.4 is a major change in the design and implementation of the language. After experimenting with several different compilers, I settled on using Per Bothner's [Kawa framework](http://www.gnu.org/software/kawa/index.html) to write the 0.4 compiler. It's too early in the development of 0.4 to assess its performance, but it's reasonable to expect it to be drastically faster than the 0.2 and 0.3 versions; Kawa performs very well.

0.4 is a simpler language, but no less expressive. In ongoing development of Bard I'm always looking for ways to make it simpler while also improving its expressive power. The 0.4 documentation currently consists of a very brief overview of the core language, which is quite small.

0.4 is under active development. It's possible to use it now to write programs, but it's probably a better idea to wait for a proper release. In its current state it's not quite a Bard inmplementation; it's more of a hybrid of Bard and Scheme, with several important Bard features still awaiting implementation. You could write programs with it, but you might have to do a lot of digging around right now to figure out how to do things.
