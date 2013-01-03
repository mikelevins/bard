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

  ;;; =====================================================================
  ;;; constants
  ;;; =====================================================================
  
  (%defglobal 'undefined #!unbound)
  (%defglobal 'nothing '())
  (%defglobal 'true #t)
  (%defglobal 'false #f)

  ;;; =====================================================================
  ;;; types
  ;;; =====================================================================
  
  ;;; schemas

  (%defglobal '<alist-table>         <alist-table>)
  (%defglobal '<bignum>              <bignum>)
  (%defglobal '<boolean>             <boolean>)
  (%defglobal '<character>           <character>)
  (%defglobal '<fixnum>              <fixnum>)
  (%defglobal '<flonum>              <flonum>)
  (%defglobal '<function>            <function>)
  (%defglobal '<interpreted-method>  <interpreted-method>)
  (%defglobal '<iostream>            <iostream>)
  (%defglobal '<keyword>             <keyword>)
  (%defglobal '<null>                <null>)
  (%defglobal '<pair>                <pair>)
  (%defglobal '<primitive-procedure> <primitive-procedure>)
  (%defglobal '<primitive>           <primitive>)
  (%defglobal '<ratnum>              <ratnum>)
  (%defglobal '<string>              <string>)
  (%defglobal '<symbol>              <symbol>)
  (%defglobal '<undefined>           <undefined>)
  ;;;  (%defglobal '<record> <record>)

  ;;; classes

  (%defglobal 'Anything     Anything) 
  (%defglobal 'Boolean      Boolean)
  (%defglobal 'Character    Character)
  (%defglobal 'Float        Float)
  (%defglobal 'Fraction     Fraction)
  (%defglobal 'Function     Function)    
  (%defglobal 'InputStream  InputStream) 
  (%defglobal 'Integer      Integer)
  (%defglobal 'Keyword      Keyword)
  (%defglobal 'List         List)
  (%defglobal 'Method       Method)
  (%defglobal 'Null         Null)
  (%defglobal 'Number       Number)
  (%defglobal 'OutputStream OutputStream)
  (%defglobal 'Pair         Pair)
  (%defglobal 'Ratio        Ratio)
  (%defglobal 'Schema       Schema)
  (%defglobal 'Stream       Stream)
  (%defglobal 'Symbol       Symbol)
  (%defglobal 'Table        Table)
  (%defglobal 'Text         Text)
  (%defglobal 'Undefined    Undefined)

  ;;; protocols

  (%defglobal 'Applying       Applying)
  (%defglobal 'Calculating    Calculating)
  (%defglobal 'Comparing      Comparing)
  (%defglobal 'Constructing   Constructing)
  (%defglobal 'Listing        Listing)
  (%defglobal 'Mapping        Mapping)
  (%defglobal 'Ordering       Ordering)
  (%defglobal 'Pairing        Pairing)
  (%defglobal 'Reading        Reading)
  (%defglobal 'TextProcessing TextProcessing)
  (%defglobal 'Typing         Typing)
  (%defglobal 'Writing        Writing)

;;; =====================================================================
;;; Protocol functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; Protocol: Applying
;;; ---------------------------------------------------------------------

  (%defglobal 'applicable? prim:applicable?)
  (%defglobal 'apply prim:apply)
  (%defglobal 'complement prim:complement)
  (%defglobal 'constantly prim:constantly)
  (%defglobal 'eval prim:eval)
  (%defglobal 'flip prim:flip)
  (%defglobal 'identity prim:identity)
  (%defglobal 'partial prim:partial)

;;; ---------------------------------------------------------------------
;;; protocol: Calculating
;;; ---------------------------------------------------------------------

  (%defglobal 'prim:* prim:*)
  (%defglobal 'prim:+ prim:+)
  (%defglobal 'prim:- prim:-)
  (%defglobal 'prim:/ prim:/)
  (%defglobal 'prim:even? prim:even?)
  (%defglobal 'prim:odd? prim:odd?)
  (%defglobal 'prim:random prim:random)

;;; ---------------------------------------------------------------------
;;; protocol: Comparing
;;; ---------------------------------------------------------------------

  (%defglobal '= bard:=)
  (%defglobal 'prim:= prim:=)

;;; ---------------------------------------------------------------------
;;; Protocol: Converting
;;; ---------------------------------------------------------------------

  (%defglobal 'as bard:as)

;;; ---------------------------------------------------------------------
;;; Protocol: Listing
;;; ---------------------------------------------------------------------

  (%defglobal 'add-first bard:add-first)
  (%defglobal 'add-last bard:add-last)
  (%defglobal 'any bard:any)
  (%defglobal 'append bard:append)
  (%defglobal 'by bard:by)
  (%defglobal 'contains? bard:contains?)
  (%defglobal 'difference bard:difference)
  (%defglobal 'drop bard:drop)
  (%defglobal 'drop-before bard:drop-before)
  (%defglobal 'element bard:element)
  (%defglobal 'empty? bard:empty?)
  (%defglobal 'every? bard:every?)
  (%defglobal 'filter bard:filter)
  (%defglobal 'find bard:find)
  (%defglobal 'first bard:first)
  (%defglobal 'head bard:head)
  (%defglobal 'interleave bard:interleave)
  (%defglobal 'interpose bard:interpose)
  (%defglobal 'intersection bard:intersection)
  (%defglobal 'join-strings bard:join-strings)
  (%defglobal 'last bard:last)
  (%defglobal 'length bard:length)
  (%defglobal 'list prim:list)
  (%defglobal 'list? bard:list?)
  (%defglobal 'map bard:map)
  (%defglobal 'merge bard:merge)
  (%defglobal 'next-last bard:next-last)
  (%defglobal 'partition bard:partition)
  (%defglobal 'position bard:position)
  (%defglobal 'range bard:range)
  (%defglobal 'reduce bard:reduce)
  (%defglobal 'repeat bard:repeat)
  (%defglobal 'rest bard:rest)
  (%defglobal 'reverse bard:reverse)
  (%defglobal 'second bard:second)
  (%defglobal 'second bard:second)
  (%defglobal 'select bard:select)
  (%defglobal 'shuffle bard:shuffle)
  (%defglobal 'slice bard:slice)
  (%defglobal 'some? bard:some?)
  (%defglobal 'sort bard:sort)
  (%defglobal 'split-string bard:split-string)
  (%defglobal 'tails bard:tails)
  (%defglobal 'take bard:take)
  (%defglobal 'take-before bard:take-before)
  (%defglobal 'take-by bard:take-by)
  (%defglobal 'unique bard:unique)
  (%defglobal 'unzip bard:unzip)
  (%defglobal 'zip bard:zip)

;;; ---------------------------------------------------------------------
;;; protocol: Mapping
;;; ---------------------------------------------------------------------

  (%defglobal 'contains-key? bard:contains-key?)
  (%defglobal 'contains-value? bard:contains-value?)
  (%defglobal 'get bard:get)
  (%defglobal 'keys bard:keys)
  (%defglobal 'merge bard:merge)
  (%defglobal 'put bard:put)
  (%defglobal 'table prim:table)
  (%defglobal 'vals bard:vals)

;;; ---------------------------------------------------------------------
;;; protocol: Ordering
;;; ---------------------------------------------------------------------

  (%defglobal 'prim:< bard:<)
  (%defglobal 'prim:<= bard:<=)
  (%defglobal 'prim:> bard:>)
  (%defglobal 'prim:>= bard:>=)

;;; ---------------------------------------------------------------------
;;; protocol: Pairing
;;; ---------------------------------------------------------------------

  (%defglobal 'left bard:left)
  (%defglobal 'pair prim:pair)
  (%defglobal 'right bard:right)

;;; ---------------------------------------------------------------------
;;; protocol: Reading
;;; ---------------------------------------------------------------------

  (%defglobal 'current-input prim:current-input)
  (%defglobal 'load %bard-load)
  (%defglobal 'read prim:read)
  (%defglobal 'read-file prim:read-file)
  (%defglobal 'read-line prim:read-line)
  (%defglobal 'read-lines prim:read-lines)
  (%defglobal 'read-text prim:read-text)

;;; ---------------------------------------------------------------------
;;; Protocol: System
;;; ---------------------------------------------------------------------

  (%defglobal 'error prim:error)
  (%defglobal 'exit prim:exit)
  (%defglobal 'gc prim:gc)
  (%defglobal 'gensym prim:gensym)
  (%defglobal 'quit prim:quit)
  (%defglobal 'room prim:room)
  (%defglobal 'uuid prim:uuid)
  (%defglobal 'version prim:version)

;;; ---------------------------------------------------------------------
;;; protocol: Typing
;;; ---------------------------------------------------------------------

  (%defglobal 'boolean? bard:boolean?)
  (%defglobal 'char? bard:char?)
  (%defglobal 'defined? bard:defined?)
  (%defglobal 'false? bard:false?)
  (%defglobal 'float? bard:float?)
  (%defglobal 'foreign-value? bard:foreign-value?)
  (%defglobal 'function? bard:function?)
  (%defglobal 'input-stream? bard:input-stream?)
  (%defglobal 'integer? bard:integer?)
  (%defglobal 'iostream? bard:iostream?)
  (%defglobal 'keyword? prim:keyword?)
  (%defglobal 'nothing? bard:nothing?)
  (%defglobal 'output-stream? bard:output-stream?)
  (%defglobal 'ratio? bard:ratio?)
  (%defglobal 'something? bard:something?)
  (%defglobal 'symbol? bard:symbol?)
  (%defglobal 'table? bard:table?)
  (%defglobal 'text? bard:text?)
  (%defglobal 'true? bard:true?)
  (%defglobal 'undefined? bard:undefined?)

;;; ---------------------------------------------------------------------
;;; protocol: Writing
;;; ---------------------------------------------------------------------

  (%defglobal 'current-output prim:current-output)
  (%defglobal 'display prim:display)
  (%defglobal 'newline prim:newline)
  (%defglobal 'print prim:print)
  (%defglobal 'show bard:show)
  (%defglobal 'write bard:write)

  )
