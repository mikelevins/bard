;;;; ***********************************************************************
;;;;
;;;; Name:          core.cljs
;;;; Project:       bard: a lisp
;;;; Purpose:       main program
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(ns bard.core
  (:require [reagent.core :as reagent :refer [atom]]))

;;; ---------------------------------------------------------------------
;;; dev-time conveniences
;;; ---------------------------------------------------------------------

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;;; ---------------------------------------------------------------------
;;; bard vm
;;; ---------------------------------------------------------------------

(defn defaultvm []
  {:version "Bard 0.4.3"
   :method nil
   :globals {}
   :env []
   :code []
   :pc 0
   :instr nil
   :stack []
   })

(defn testvm []
  {:version "Bard 0.4.3"
   :method nil
   :globals {}
   :env []
   :code [[:halt]]
   :pc 0
   :instr nil
   :stack [0]
   })

;;; bard vm state
(def bardvm (atom (defaultvm)))

;;; ---------------------------------------------------------------------
;;; opcodes
;;; ---------------------------------------------------------------------

(def opcodes
  {:halt (fn [& _]
           (if (empty? (:stack @bardvm))
             nil
             (first (:stack @bardvm))))})

(defn lookup-opcode [opc]
  (opc opcodes))

;;; ---------------------------------------------------------------------
;;; instructions
;;; ---------------------------------------------------------------------

(defn instruction-operator [instr]
  (first instr))

(defn instruction-arguments [instr]
  (rest instr))

(defn fetch! []
  (swap! bardvm
         (fn [state]
           {:method (:method state)
            :globals (:globals state)
            :env (:env state)
            :code (:code state)
            :pc (:pc state)
            :instr (nth (:code state)
                        (:pc state))
            :stack (:stack state)})))

(defn exec! []
  (let [instr (:instr @bardvm)
        opcode (instruction-operator instr)
        op (lookup-opcode opcode)
        args (instruction-arguments instr)]
    (apply op args)))

;;; ---------------------------------------------------------------------
;;; page utils
;;; ---------------------------------------------------------------------

(defn id->element [id]
  (js/document.getElementById id))

;;; ---------------------------------------------------------------------
;;; page constants
;;; ---------------------------------------------------------------------
;;; ---------------------------------------------------------------------
;;; element styles
;;; ---------------------------------------------------------------------

(def $bard-banner-style
  {:color "black"
   :font-family "Source Code Pro"
   :font-weight "bold"})

;;; ---------------------------------------------------------------------
;;; helper functions 
;;; ---------------------------------------------------------------------
;;; ---------------------------------------------------------------------
;;; page 
;;; ---------------------------------------------------------------------

(defn bard []
  [:div {:id "bard"
         :style $bard-banner-style}
   [:h1 "Bard 0.4.3"]])

;;; ---------------------------------------------------------------------
;;; page rendering 
;;; ---------------------------------------------------------------------

(reagent/render-component [bard]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your vm to force rerendering depending on
  ;; your application
  ;; (swap! vm update-in [:__figwheel_counter] inc)
  )
