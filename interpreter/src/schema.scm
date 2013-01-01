;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.scm
;;;; Project:       Bard
;;;; Purpose:       representation of types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------

;;; a schema is a concrete description of a datatype--that is, a
;;; description of how bits and bytes are laid out.  In bard, a schema
;;; is a value.  If you ask Bard for the type of a value, the object
;;; it returns is a schema.

;;; Behind the scenes, in the Scheme code that implements Bard, there
;;; are several kinds of schema, one for each of the following kinds
;;; of value:

;;; 1. primitive values
;;;    - scheme values
;;;    - built into gambit
;;;    - not gambit structures
;;;    - examples: fixnums, booleans, strings
;;; 2. structure values
;;;    - scheme values
;;;    - built into gambit
;;;    - represented by gambit structures
;;;    - examples: device ports
;;; 3. base values
;;;    - bard values
;;;    - built into bard
;;;    - not built into gambit
;;;    - represented by gambit structures
;;;    - examples: alist-tables, bard functions and methods, type objects
;;; 4. records (values that consist of named fields)
;;;    - bard values
;;;    - not built into bard
;;;    - not built into gambit
;;;    - represented by gambit structures
;;;    - examples: user-defined records
;;; 5. tuples (values that consist of numerically-indexed fields)
;;;    - bard values
;;;    - not built into bard
;;;    - not built into gambit
;;;    - represented by gambit structures
;;;    - examples: user-defined tuples
;;; 5. unions
;;;    - bard values
;;;    - not built into bard
;;;    - not built into gambit
;;;    - represented by gambit structures
;;;    - examples: user-defined types whose values may be instances of
;;;      any of the various kinds of schemas
;;; 5. foreign values
;;;    - C values
;;;    - not built into bard
;;;    - not built into gambit
;;;    - represented by gambit foreign pointers
;;;    - examples: C strings, C arrays, file descriptors

;;; some schemas are tagged; others are not.
;;; primitive, structure, and base schemas are tagged, meaning
;;; that an integer tag is globally assigned to identify
;;; the schema, enabling the Bard runtime to efficiently
;;; discriminate between instances of each of these schemas.

;;; tags are recovered from instances of primitive schemas
;;; by gambit's built-in tag-handling primitives.

;;; they are recovered from instances of structure schemas
;;; by looking them up in a global registry.

;;; they are stored in instaces of base schemas, and recovered
;;; by the bard runtime's base-schema API.

;;; record, tuple, union, and foreign schemas are untagged.  in other
;;; words, a unique tag is not assigned to each of these types. THe
;;; reason is that these are open types that can be arbitrarily
;;; extended by user of bard; there is no practical way to ensure that
;;; tags for these types could be unique.

;;; instead, for each of these kinds of schema there is a single
;;; unique tag assigned to all schemas of that type, so that the bard
;;; runtime can efficiently determine the kind of schema the value
;;; belongs to.

;;; for foreign values, this tag is the "foreign" tag assigned to
;;; foreign values by the gambit runtime. Different foreign types are
;;; distinguished by the type-names assigned to them by the gambt
;;; runtime.

;;; for the others--record, tuple, and union schemas--the tag is
;;; defined in the definition of the schema type, and the constructor
;;; for the type arranges for each instance to carry that tag value.

;;; when a tagged schema is defined, it must be registered so that the
;;; bard runtime can correctly identify the schema object that
;;; corresponds to its tag.

;;; instances of an untagged schema carry a direct reference to their
;;; schema in the body of the value, so registration is not
;;; needed. The exception is foreign-schema: a foreign-schema refers
;;; to a set of foreign types. foreign-schemas have their own registry
;;; mapping foreign type names to corresponding schema objects.

;;; some schemas are instantiable; others are not.  the base, record,
;;; tuple, and union schemas represent types whose instances are
;;; dynamically constructed by bard code by allocating and
;;; initializing instances. For each of these schemas, there is a
;;; corresponding instance type. 

;;; For example, records are instantiable. When an instance of a
;;; record is needed, Bard allocates a record-instance value, then
;;; initializes it according to the rules specified by the record and
;;; the arguments to the constructor.

;;; Non-instantiable schemas do not have accompanying instance types.
;;; For example, <pair> is a primitive schema. When a new instance of
;;; <pair> is needed, Bard calls the underlying Scheme's cons function
;;; to create it. There's no pair-instance type; none is needed,
;;; because the pair type built into gambit serves the purpose of an
;;; instance type for <pair>.

;;; all of the untagged schemas except foreign-schema are
;;; instantiable. None of the tagged schemas except base-schema are
;;; instantiable. Instance types for base-schemas are defined along
;;; with the creation of the base-schema values in the schemas-*.scm
;;; files.

(define-type schema extender: define-schema name tag)
(define-type schema-instance extender: define-instance name schema)

;;; tagged schemas

(define-schema primitive-schema)
(define-schema structure-schema prototype)
(define-schema base-schema)

;;; registry for tagged schemas
(define +tag->schema-registry+ (make-table test: eqv?))

(define (%register-tagged-schema! sc tag)
  (table-set! +tag->schema-registry+ tag sc))

(define (%tag->registered-schema tag)
  (table-ref +tag->schema-registry+ tag #f))

;;; untagged schemas

(define-schema record-schema)
(define-instance record-instance slots)
(define-schema tuple-schema)
(define-instance tuple-instance elements)
(define-schema union-schema)
(define-instance union-instance variants)

;;; registry for foreign schemas
(define +foreign-name->schema-registry+ (make-table test: eqv?))

(define-schema foreign-schema)

;;; ----------------------------------------------------------------------
;;; base schemas
;;; ----------------------------------------------------------------------
;;; here we define the basis for each base schema including:
;;; - tag
;;; - type object
;;; - instance type
;;; - primitive constructors
;;; - primitive accessors
;;; all other related APIs are defined in protocol files

;;; alist table
;;; ----------------------------------------------------------------------

(define tags:$bard-alist-table (next-bard-structure-tag))
(define <alist-table> (make-base-schema '<alist-table> tags:$bard-alist-table))

(define-instance alist-table-instance
  constructor: make-alist-table-instance
  (slots alist-table-instance-slots set-alist-table-instance-slots!))

;;; constructor
;;; accessors

;;; function
;;; ----------------------------------------------------------------------
;;; constructor
;;; accessors

;;; method
;;; ----------------------------------------------------------------------
;;; constructor
;;; accessors

;;; primitive
;;; ----------------------------------------------------------------------
;;; constructor
;;; accessors