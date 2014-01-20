### Parts of a type:

A type is composed of three parts:

* a class
* structures that are members of the class
* protocols that mention the class

Type = class + protocols + structures

* Class

  A value that identifies a defined set of member types and a defined
  set of supertypes. 

* Protocol

  A set of functions. A protocol is part of the type associated with a
  class if the protocol's functions mention the class as inputs or
  outputs.

* Structure

  A data definition. A structure defines how to construct a set of
  values that are known as instances of the structure. The elements
  defined by a structure are:

  * slots contained by structure instances
  * a constructor for instances
  * a predicate that can determine whether an arbitrary value is an
    instance of the structure
  * a set of accessors that can read and optionally write instance slots

There are two ways to cause a given structure to become a member of a class:

1. Assert membership explicitly using add-member!

2. Assert membership implicitly using define method to specialize a
   protocol function on the structure.



