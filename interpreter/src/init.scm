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
  (%defglobal 'drop bard:drop)
  (%defglobal 'element bard:element)
  (%defglobal 'empty? bard:empty?)
  (%defglobal 'filter bard:filter)
  (%defglobal 'first bard:first)
  (%defglobal 'join-strings bard:join-strings)
  (%defglobal 'last bard:last)
  (%defglobal 'length bard:length)
  (%defglobal 'list prim:list)
  (%defglobal 'list? bard:list?)
  (%defglobal 'map bard:map)
  (%defglobal 'prim:alist-table-merge prim:alist-table-merge)
  (%defglobal 'next-last bard:next-last)
  (%defglobal 'reduce bard:reduce)
  (%defglobal 'rest bard:rest)
  (%defglobal 'second bard:second)
  (%defglobal 'some? bard:some?)
  (%defglobal 'split-string bard:split-string)
  (%defglobal 'take bard:take)
  (%defglobal 'take-by bard:take-by)

;;; ---------------------------------------------------------------------
;;; protocol: Mapping
;;; ---------------------------------------------------------------------

  (%defglobal 'prim:alist-table-contains-key? prim:alist-table-contains-key?)
  (%defglobal 'prim:alist-table-contains-value? prim:alist-table-contains-value?)
  (%defglobal 'prim:alist-table-get prim:alist-table-get)
  (%defglobal 'prim:alist-table-keys prim:alist-table-keys)
  (%defglobal 'prim:alist-table-merge prim:alist-table-merge)
  (%defglobal 'prim:alist-table-put prim:alist-table-put)
  (%defglobal 'table prim:table)
  (%defglobal 'prim:alist-table-vals prim:alist-table-vals)

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

  (%defglobal 'boolean? prim:boolean?)
  (%defglobal 'char? prim:char?)
  (%defglobal 'false? prim:false?)
  (%defglobal 'float? prim:float?)
  (%defglobal 'foreign-value? prim:foreign-value?)
  (%defglobal 'function? prim:function?)
  (%defglobal 'input-stream? prim:input-stream?)
  (%defglobal 'integer? prim:integer?)
  (%defglobal 'iostream? prim:iostream?)
  (%defglobal 'nothing? bard:nothing?)
  (%defglobal 'output-stream? prim:output-stream?)
  (%defglobal 'symbol? bard:symbol?)
  (%defglobal 'table? bard:table?)
  (%defglobal 'text? bard:text?)
  (%defglobal 'true? prim:true?)

;;; ---------------------------------------------------------------------
;;; protocol: Writing
;;; ---------------------------------------------------------------------

  (%defglobal 'current-output prim:current-output)
  (%defglobal 'display prim:display)
  (%defglobal 'newline prim:newline)
  (%defglobal 'print prim:print)
  (%defglobal 'show prim:show)
  (%defglobal 'write prim:write)

  )
