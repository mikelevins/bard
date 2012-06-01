;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          toplevel.scm
;;;; Project:       Bard
;;;; Purpose:       the initial Bard global environment
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%init-bard)

  (set! $bard-global-variables (%global-variables))

;;; ---------------------------------------------------------------------
;;; primitive combinators
;;; ---------------------------------------------------------------------

  (%defglobal 'complement prim:complement)
  (%defglobal 'constantly prim:constantly)
  (%defglobal 'flip prim:flip)
  (%defglobal 'partial prim:partial)
  (%defglobal 'identity prim:identity)

;;; ---------------------------------------------------------------------
;;; system primitives
;;; ---------------------------------------------------------------------

  (%defglobal 'exit prim:exit)
  (%defglobal 'gc prim:gc)
  (%defglobal 'quit prim:quit)
  (%defglobal 'room prim:room)
  (%defglobal 'version prim:version)

;;; ---------------------------------------------------------------------
;;; type-system primitives
;;; ---------------------------------------------------------------------

  (%defglobal 'Type Type)
  (%defglobal 'type? prim:type?)
  (%defglobal 'type prim:type)
  (%defglobal 'singleton prim:singleton)
  (%defglobal 'singleton? prim:singleton?)
  (%defglobal '%primitive-type-tag %type-tag)

;;; ---------------------------------------------------------------------
;;; number primitives
;;; ---------------------------------------------------------------------

  (%defglobal '+ prim:+)
  (%defglobal '- prim:-)
  (%defglobal '* prim:*)
  (%defglobal '/ prim:/)
  (%defglobal 'odd? odd?)
  (%defglobal 'even? even?)
  (%defglobal 'random prim:random)

;;; ---------------------------------------------------------------------
;;; List primitives
;;; ---------------------------------------------------------------------

  (%defglobal 'list prim:list)

;;; ---------------------------------------------------------------------
;;; Frame primitives
;;; ---------------------------------------------------------------------

  (%defglobal 'frame prim:frame)

;;; ---------------------------------------------------------------------
;;; types
;;; ---------------------------------------------------------------------

  ;; representations

  (%defglobal '<undefined> <undefined>)
  (%defglobal '<null> <null>)
  (%defglobal '<character> <character>)
  (%defglobal '<boolean> <boolean>)
  (%defglobal '<symbol> <symbol>)
  (%defglobal '<keyword> <keyword>)
  (%defglobal '<flonum> <flonum>)
  (%defglobal '<ratnum> <ratnum>)
  (%defglobal '<fixnum> <fixnum>)
  (%defglobal '<bignum> <bignum>)
  (%defglobal '<primitive-procedure> <primitive-procedure>)
  (%defglobal '<list> <list>)
  (%defglobal '<string> <string>)
  (%defglobal '<iostream> <iostream>)
  (%defglobal '<frame> <frame>)
  (%defglobal '<function> <function>)
  (%defglobal '<primitive-method> <primitive-method>)
  (%defglobal '<interpreted-method> <interpreted-method>)

  ;; constants

  (%defglobal 'undefined (%undefined))
  (%defglobal 'nothing (%nothing))
  (%defglobal 'true (%true))
  (%defglobal 'false (%false))

  ;; protocols

  (%defglobal 'Anything Anything)
  ;;(%defglobal 'something? bard:something?)

  (%defglobal 'Type Type)

  ;; (%defglobal 'Applicable Applicable)
  ;; (%defglobal 'applicable? bard:applicable?)
  ;; (%defglobal 'apply bard:apply)

  ;; (%defglobal 'As As)
  ;; (%defglobal 'as bard:as)

  ;; (%defglobal 'Atom Atom)
  ;; (%defglobal 'atom? bard:atom?)

  ;; (%defglobal 'Boolean Boolean)
  ;; (%defglobal 'boolean? bard:boolean?)
  ;; (%defglobal 'false? bard:false?)
  ;; (%defglobal 'true? bard:true?)

  ;; (%defglobal 'Character Character)
  ;; (%defglobal 'character? bard:character?)

  ;; (%defglobal 'Comparable Comparable)
  ;; (%defglobal 'comparable? bard:comparable?)
  (%defglobal '= prim:=)
  (%defglobal '> prim:>)
  (%defglobal '< prim:<)
  (%defglobal '>= prim:>=)
  (%defglobal '<= prim:<=)

  ;; (%defglobal 'Float Float)
  ;; (%defglobal 'float? bard:float?)

  ;; (%defglobal 'ForeignValue ForeignValue)
  ;; (%defglobal 'foreign-value? bard:foreign-value?)

  ;; (%defglobal 'Frame Frame)
  ;; (%defglobal 'frame? bard:frame?)
  ;; (%defglobal 'contains-key? bard:contains-key?)
  ;; (%defglobal 'contains-value? bard:contains-value?)
  ;; (%defglobal 'get bard:get)
  ;; (%defglobal 'keys bard:keys)
  ;; (%defglobal 'merge bard:merge)
  ;; (%defglobal 'put bard:put)
  ;; (%defglobal 'vals bard:vals)

  ;; (%defglobal 'Function Function)
  ;; (%defglobal 'function? bard:function?)

  ;; (%defglobal 'Integer Integer)
  ;; (%defglobal 'integer? bard:integer?)

  ;; (%defglobal 'IOStream IOStream)
  ;; (%defglobal 'close bard:close)
  ;; (%defglobal 'current-input bard:current-input)
  ;; (%defglobal 'current-output bard:current-output)
  ;; (%defglobal 'display bard:display)
  ;; (%defglobal 'input-stream? bard:input-stream?)
  ;; (%defglobal 'iostream? bard:iostream?)
  (%defglobal 'load %bard-load)
  ;; (%defglobal 'open bard:open)
  ;; (%defglobal 'output-stream? bard:output-stream?)
  (%defglobal 'print prim:print)
  (%defglobal 'read prim:read)
  ;; (%defglobal 'read-file bard:read-file)
  ;; (%defglobal 'read-line bard:read-line)
  ;; (%defglobal 'read-lines bard:read-lines)
  ;; (%defglobal 'show bard:show)
  ;; (%defglobal 'write bard:write)

  ;; (%defglobal 'Keyword Keyword)
  ;; (%defglobal 'keyword? bard:keyword?)

  ;; (%defglobal 'List List)
  ;; (%defglobal 'add-first bard:add-first)
  ;; (%defglobal 'add-last bard:add-last)
  ;; (%defglobal 'any bard:any)
  ;; (%defglobal 'append bard:append)
  ;; (%defglobal 'contains? bard:contains?)
  ;; (%defglobal 'difference bard:difference)
  ;; (%defglobal 'drop bard:drop)
  ;; (%defglobal 'drop-before bard:drop-before)
  ;; (%defglobal 'element bard:element)
  ;; (%defglobal 'empty? bard:empty?)
  ;; (%defglobal 'every? bard:every?)
  ;; (%defglobal 'filter bard:filter)
  ;; (%defglobal 'find bard:find)
  ;; (%defglobal 'first bard:first)
  ;; (%defglobal 'head bard:head)
  ;; (%defglobal 'interleave bard:interleave)
  ;; (%defglobal 'interpose bard:interpose)
  ;; (%defglobal 'intersection bard:intersection)
  ;; (%defglobal 'last bard:last)
  ;; (%defglobal 'length bard:length)
  ;; (%defglobal 'list? bard:list?)
  ;; (%defglobal 'map bard:map)
  ;; (%defglobal 'merge bard:merge)
  ;; (%defglobal 'partition bard:partition)
  ;; (%defglobal 'position bard:position)
  ;; (%defglobal 'range bard:range)
  ;; (%defglobal 'reduce bard:reduce)
  ;; (%defglobal 'repeat bard:repeat)
  ;; (%defglobal 'reverse bard:reverse)
  ;; (%defglobal 'second bard:second)
  ;; (%defglobal 'select bard:select)
  ;; (%defglobal 'shuffle bard:shuffle)
  ;; (%defglobal 'slice bard:slice)
  ;; (%defglobal 'some? bard:some?)
  ;; (%defglobal 'sort bard:sort)
  ;; (%defglobal 'tail bard:tail)
  ;; (%defglobal 'tails bard:tails)
  ;; (%defglobal 'take bard:take)
  ;; (%defglobal 'take-before bard:take-before)
  ;; (%defglobal 'unique bard:unique)
  ;; (%defglobal 'unzip bard:unzip)
  ;; (%defglobal 'zip bard:zip)

  ;; (%defglobal 'Method Method)
  ;; (%defglobal 'method? bard:method?)

  ;; (%defglobal 'Name Name)
  ;; (%defglobal 'name? bard:name?)

  ;; (%defglobal 'Null Null)
  ;; (%defglobal 'nothing? bard:nothing?)

  ;; (%defglobal 'Number Number)
  ;; (%defglobal 'number? bard:number?)

  ;; (%defglobal 'PrimitiveValue PrimitiveValue)
  ;; (%defglobal 'primitive-value? bard:primitive-value?)

  ;; (%defglobal 'Procedure Procedure)
  ;; (%defglobal 'procedure? bard:procedure?)

  ;; (%defglobal 'Ratio Ratio)
  ;; (%defglobal 'ratio? bard:ratio?)

  ;; (%defglobal 'StructureValue StructureValue)
  ;; (%defglobal 'structure-value? bard:structure-value?)

  ;; (%defglobal 'Symbol Symbol)
  ;; (%defglobal 'symbol? bard:symbol?)

  ;; (%defglobal 'Text Text)
  ;; (%defglobal 'text? bard:text?)

  ;; (%defglobal 'Undefined Undefined)
  ;; (%defglobal 'undefined? bard:undefined?)

  )
