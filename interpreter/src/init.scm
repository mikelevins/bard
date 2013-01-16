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
  (%defglobal '<base-schema>         <base-schema>)
  (%defglobal '<bignum>              <bignum>)
  (%defglobal '<boolean>             <boolean>)
  (%defglobal '<character>           <character>)
  (%defglobal '<class>               <class>)
  (%defglobal '<fixnum>              <fixnum>)
  (%defglobal '<flonum>              <flonum>)
  (%defglobal '<foreign-schema>      <foreign-schema>)
  (%defglobal '<function>            <function>)
  (%defglobal '<generator>           <generator>)
  (%defglobal '<interpreted-method>  <interpreted-method>)
  (%defglobal '<iostream>            <iostream>)
  (%defglobal '<keyword>             <keyword>)
  (%defglobal '<null>                <null>)
  (%defglobal '<pair>                <pair>)
  (%defglobal '<primitive-procedure> <primitive-procedure>)
  (%defglobal '<primitive-schema>    <primitive-schema>)
  (%defglobal '<primitive>           <primitive>)
  (%defglobal '<protocol>            <protocol>)
  (%defglobal '<ratnum>              <ratnum>)
  (%defglobal '<record>              <record>)
  (%defglobal '<string>              <string>)
  (%defglobal '<structure-schema>    <structure-schema>)
  (%defglobal '<symbol>              <symbol>)
  (%defglobal '<tuple>               <tuple>)
  (%defglobal '<undefined>           <undefined>)
  (%defglobal '<union>               <union>)

  ;;; classes

  (%defglobal '&            &) 
  (%defglobal 'Anything     Anything) 
  (%defglobal 'Applicable   Applicable) 
  (%defglobal 'Boolean      Boolean)
  (%defglobal 'Character    Character)
  (%defglobal 'Class        Class)
  (%defglobal 'File         File)
  (%defglobal 'Float        Float)
  (%defglobal 'Fraction     Fraction)
  (%defglobal 'Function     Function)    
  (%defglobal 'InputStream  InputStream) 
  (%defglobal 'Integer      Integer)
  (%defglobal 'IODirection  IODirection)
  (%defglobal 'IOMode       IOMode)
  (%defglobal 'IOType       IOType)
  (%defglobal 'Keyword      Keyword)
  (%defglobal 'List         List)
  (%defglobal 'Method       Method)
  (%defglobal 'Null         Null)
  (%defglobal 'Number       Number)
  (%defglobal 'Orderable    Orderable)
  (%defglobal 'OutputStream OutputStream)
  (%defglobal 'Pair         Pair)
  (%defglobal 'Protocol     Protocol)
  (%defglobal 'Ratio        Ratio)
  (%defglobal 'Schema       Schema)
  (%defglobal 'Stream       Stream)
  (%defglobal 'Symbol       Symbol)
  (%defglobal 'Table        Table)
  (%defglobal 'Text         Text)
  (%defglobal 'Type         Type)
  (%defglobal 'Undefined    Undefined)
  (%defglobal 'URL          URL)

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
  (%defglobal 'Streaming      Streaming)
  (%defglobal 'TextProcessing TextProcessing)
  (%defglobal 'Typing         Typing)
  (%defglobal 'Writing        Writing)

;;; =====================================================================
;;; Protocol functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; Protocol: Addressing
;;; ---------------------------------------------------------------------

  (%defglobal 'url bard:url)
  (%defglobal 'url-domain bard:url-domain)
  (%defglobal 'url-path bard:url-path)
  (%defglobal 'url-port bard:url-port)
  (%defglobal 'url-query bard:url-query)
  (%defglobal 'url-scheme bard:url-scheme)

;;; ---------------------------------------------------------------------
;;; Protocol: Applying
;;; ---------------------------------------------------------------------

  (%defglobal 'applicable? prim:applicable?)
  (%defglobal 'apply prim:apply)
  (%defglobal 'complement prim:complement)
  (%defglobal 'compose prim:compose)
  (%defglobal 'constantly prim:constantly)
  (%defglobal 'eval prim:eval)
  (%defglobal 'flip prim:flip)
  (%defglobal 'identity prim:identity)
  (%defglobal 'partial prim:partial)

;;; ---------------------------------------------------------------------
;;; protocol: Calculating
;;; ---------------------------------------------------------------------

  (%defglobal '* prim:*)
  (%defglobal '+ prim:+)
  (%defglobal '- prim:-)
  (%defglobal '/ prim:/)
  (%defglobal 'even? prim:even?)
  (%defglobal 'odd? prim:odd?)
  (%defglobal 'random prim:random)
  (%defglobal 'remainder prim:remainder)
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
;;; Protocol: Creating
;;; ---------------------------------------------------------------------

  (%defglobal 'make bard:make)

;;; ---------------------------------------------------------------------
;;; Protocol: Generating
;;; ---------------------------------------------------------------------

  (%defglobal 'next prim:next)
  (%defglobal 'cycle prim:cycle)
  (%defglobal 'iterate prim:iterate)
  (%defglobal 'range-from prim:range-from)


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
  (%defglobal 'join-text bard:join-text)
  (%defglobal 'last bard:last)
  (%defglobal 'length bard:length)
  (%defglobal 'list prim:list)
  (%defglobal 'list? bard:list?)
  (%defglobal 'map prim:map)
  (%defglobal 'next-last bard:next-last)
  (%defglobal 'partition prim:partition)
  (%defglobal 'prim:alist-table-merge prim:alist-table-merge)
  (%defglobal 'range bard:range)
  (%defglobal 'reduce bard:reduce)
  (%defglobal 'rest bard:rest)
  (%defglobal 'second bard:second)
  (%defglobal 'some? bard:some?)
  (%defglobal 'split-text bard:split-text)
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

  (%defglobal '< bard:<)
  (%defglobal '<= bard:<=)
  (%defglobal '> bard:>)
  (%defglobal '>= bard:>=)

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
  (%defglobal 'singleton singleton)
  (%defglobal 'something? bard:something?)
  (%defglobal 'symbol? bard:symbol?)
  (%defglobal 'table? bard:table?)
  (%defglobal 'text? bard:text?)
  (%defglobal 'true? prim:true?)
  (%defglobal 'type bard:type)

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
