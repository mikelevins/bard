;;;; ***********************************************************************
;;;;
;;;; Name:          toplevel.scm
;;;; Project:       Bard
;;;; Purpose:       the initial Bard global environment
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

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
  
  ;;; structures

  (%defglobal '<alist-table>         <alist-table>)
  (%defglobal '<base-structure>         <base-structure>)
  (%defglobal '<bignum>              <bignum>)
  (%defglobal '<boolean>             <boolean>)
  (%defglobal '<character>           <character>)
  (%defglobal '<class>               <class>)
  (%defglobal '<fixnum>              <fixnum>)
  (%defglobal '<flonum>              <flonum>)
  (%defglobal '<foreign-structure>   <foreign-structure>)
  (%defglobal '<function>            <function>)
  (%defglobal '<generator>           <generator>)
  (%defglobal '<interpreted-method>  <interpreted-method>)
  (%defglobal '<iostream>            <iostream>)
  (%defglobal '<keyword>             <keyword>)
  (%defglobal '<null>                <null>)
  (%defglobal '<pair>                <pair>)
  (%defglobal '<primitive-procedure> <primitive-procedure>)
  (%defglobal '<primitive-structure> <primitive-structure>)
  (%defglobal '<primitive>           <primitive>)
  (%defglobal '<protocol>            <protocol>)
  (%defglobal '<ratnum>              <ratnum>)
  (%defglobal '<string>              <string>)
  (%defglobal '<structure-structure> <structure-structure>)
  (%defglobal '<symbol>              <symbol>)
  (%defglobal '<undefined>           <undefined>)
  (%defglobal '<url>                 <url>)
  (%defglobal '<vector>              <vector>)

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
  (%defglobal 'Structure    Structure)
  (%defglobal 'Stream       Stream)
  (%defglobal 'Symbol       Symbol)
  (%defglobal 'Table        Table)
  (%defglobal 'Text         Text)
  (%defglobal 'Type         Type)
  (%defglobal 'Undefined    Undefined)
  (%defglobal 'URL          URL)

  ;;; protocols

  (%defglobal 'Addressing     Addressing)
  (%defglobal 'Applying       Applying)
  (%defglobal 'Calculating    Calculating)
  (%defglobal 'Comparing      Comparing)
  (%defglobal 'Converting     Converting)
  (%defglobal 'Creating       Creating)
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
  (%defglobal 'max prim:max)
  (%defglobal 'min prim:min)
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

  (%defglobal 'cycle prim:cycle)
  (%defglobal 'generated-count prim:generated-count)
  (%defglobal 'generated-values prim:generated-values)
  (%defglobal 'iterate prim:iterate)
  (%defglobal 'next prim:next)
  (%defglobal 'next-n prim:next-n)
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
  (%defglobal 'map prim:map)
  (%defglobal 'member? bard:member?)
  (%defglobal 'next-last bard:next-last)
  (%defglobal 'partition prim:partition)
  (%defglobal 'position bard:position)
  (%defglobal 'position-if bard:position-if)
  (%defglobal 'range bard:range)
  (%defglobal 'reduce prim:reduce)
  (%defglobal 'rest bard:rest)
  (%defglobal 'reverse bard:reverse)
  (%defglobal 'second bard:second)
  (%defglobal 'some? bard:some?)
  (%defglobal 'split-text bard:split-text)
  (%defglobal 'take bard:take)
  (%defglobal 'take-by bard:take-by)
  (%defglobal 'vector prim:vector)

;;; ---------------------------------------------------------------------
;;; protocol: Mapping
;;; ---------------------------------------------------------------------

  (%defglobal 'get prim:get)
  (%defglobal 'get-key bard:get-key)
  (%defglobal 'keys bard:keys)
  (%defglobal 'merge bard:merge)
  (%defglobal 'put prim:put)
  (%defglobal 'put-key bard:put-key)
  (%defglobal 'vals bard:vals)
  (%defglobal 'dict prim:table)

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
;;; protocol: Streaming
;;; ---------------------------------------------------------------------

  (%defglobal 'contents bard:contents)
  (%defglobal 'lines bard:lines)
  (%defglobal 'stream-direction bard:stream-direction)

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
  (%defglobal 'character? prim:char?)
  (%defglobal 'class? bard:class?)
  (%defglobal 'false? prim:false?)
  (%defglobal 'float? prim:float?)
  (%defglobal 'foreign-value? prim:foreign-value?)
  (%defglobal 'function? prim:function?)
  (%defglobal 'input-stream? prim:input-stream?)
  (%defglobal 'integer? prim:integer?)
  (%defglobal 'iostream? prim:iostream?)
  (%defglobal 'keyword? bard:keyword?)
  (%defglobal 'nothing? bard:nothing?)
  (%defglobal 'output-stream? prim:output-stream?)
  (%defglobal 'protocols prim:protocols)
  (%defglobal 'list? bard:list?)
  (%defglobal 'list-protocols prim:list-protocols)
  (%defglobal 'method? bard:method?)
  (%defglobal 'pair? bard:pair?)
  (%defglobal 'protocol? bard:protocol?)
  (%defglobal 'singleton prim:singleton)
  (%defglobal 'something? bard:something?)
  (%defglobal 'symbol? bard:symbol?)
  (%defglobal 'table? bard:table?)
  (%defglobal 'text? bard:text?)
  (%defglobal 'true? prim:true?)
  (%defglobal 'type bard:type)
  (%defglobal 'undefined? bard:undefined?) ;;; *** unimplemented

;;; ---------------------------------------------------------------------
;;; protocol: Writing
;;; ---------------------------------------------------------------------

  (%defglobal 'current-output prim:current-output)
  (%defglobal 'display prim:display)
  (%defglobal 'newline prim:newline)
  (%defglobal 'print prim:print)
  (%defglobal 'show prim:show)
  (%defglobal 'write prim:write)

;;; ---------------------------------------------------------------------
;;; computed globals
;;; ---------------------------------------------------------------------

)

