(ns cosheet.interaction-state
  (:require [goog.dom :as gdom]
            [cosheet.dom-utils :refer [find-ancestor-with-class
                                       scroll-to-be-visible]]))

;;; These are the UI operations on the edit field and on selections. We
;;; put them in their own file so both client.cljs and ajax.cljs can
;;; access them.

(def edit-field-open-on (atom nil)) ;; The dom the edit field is open on.

(defn open-edit-field
  [target initial-content]
  (when (not= target @edit-field-open-on)
    (let [select-holder (js/document.getElementById "select_holder")
          edit-input (js/document.getElementById "edit_input")]
      (set! (.-value edit-input) (if (= (first initial-content) \u00A0)
                                   "" ;; System provided name; clear it
                                      ;; so options appear.
                                   initial-content))
      ;; Have to use setAttribute, as list is not a DOM field,
      ;; only an HTML attribute.
      (.setAttribute edit-input "list"
                     (if (.contains (.-classList target) "tag")
                       ":label-values"
                       nil))
      (.add (.-classList select-holder) "editing")
      (scroll-to-be-visible select-holder)
      (.focus edit-input)
      (.select edit-input)
      (reset! edit-field-open-on target))))

(defn close-edit-field
  "Close the edit field, without storing the value."
  []
  (when @edit-field-open-on
    (let [select-holder (js/document.getElementById "select_holder")]
      (reset! edit-field-open-on nil)
      (.remove (.-classList select-holder) "editing"))))

;; The currently selected dom.
(def selected (atom nil))

;; The last valid selection request id we have received from the client,
;; if we we haven't already done it, and if the user hasn't made a
;; different selection since we got it.
(def pending-server-selection-request-id (atom nil))

(defn set-special-class
  "Set a specific dom to have a specified class, and record which dom it is.
   atom-with-current is an atom that records the dom, if any, that currently
   has the class"
  [dom atom-with-current class]
  (let [old-dom @atom-with-current]
    (when (not= dom old-dom)
      (when old-dom
        (.remove (.-classList old-dom) class))
      (when dom
        (.add (.-classList dom) class))
      (reset! atom-with-current dom))))

(defn deselect []
  (let [target @selected]
    (when target
      (.remove (.-classList target) "selected")
      ;; Put it at the end, where it will be invisible, but still findable.
      (gdom/appendChild  (js/document.getElementById "app")
                         (js/document.getElementById "select_holder"))
      (reset! selected nil))))

(defn select [target]
  (.log js/console (str "Selecting id " (.-id target) "."))
  (.log js/console (str "current selection " @selected))
  (when (not= target @selected)
    (deselect)
    (.add (.-classList target) "selected")
    (gdom/appendChild target (js/document.getElementById "select_holder"))
    (.log js/console (str "Selected id " (.-id target) "."))
    (reset! selected target)
    (scroll-to-be-visible target)))

(defn select-and-clear-pending [target]
  (select target)
  (reset! pending-server-selection-request-id nil))

