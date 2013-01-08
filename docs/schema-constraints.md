# Schema-Constraint Expressions

Bard promises to create an instance of a class for any literal
expression, but if a class is represented by more than one schema, it
doesn't guarantee which schema it will choose. Sometimes programmers
will want to ensure that a particular schema is chosen. For example,
the choice of schema can have consequences for performance, and a
programmer will prefer the schema whose performance characteristics
match the requirements of the program.

Schema-constraint expression are designed to enable a programmer to
conveniently express the requirement for a particular schema.

Suppose that a Bard implementation supports two different
representations for the class Table: <alist-table> represents the
mappings as a sequence of pairs; <wb-tree-table> represents them as a
weight-balanced binary tree.

Entering this expression:

    ? { a: 1 b: 2 }

at the Bard prompt might result in a value of either schema. If we
want to ensure that the value is of the schema <wb-tree-table>, we
decorate it with a schema constraint:

    ? #<wb-tree-table> { a: 1 b: 2 }

The Bard reader detects the constraint and constructs a value of the
specified schema. The syntax is:

    #<schema-variable> <literal-expression>

The hash mark followed by a schema variable establishes the constraint
that applies to the next-read literal expression.

There are a few rules and limitations on the use of schema
constraints. They work only on literal expressions; you can't
constrain an arbitrary S-expression. The constraint must name a schema
that is a member of the class described by the literal; you can't
constrain a literal to be a schema that isn't a member of its class;
for example, you can't constrain a text literal to be a <fixnum>. 

