(ns cosheet2.interaction-state
  (:require [cosheet2.dom-utils :refer [find-ancestor-with-class
                                        scroll-to-be-visible]]))

;;; These are the UI operations on the edit field, the context menu,
;;; and on selections. We put them in their own file so both
;;; client.cljs and ajax.cljs can access them.


;;; The dom the edit field is open on.
(def edit-field-open-on (atom nil))

(defn open-edit-field
  [target initial-content]
  (when (not= target @edit-field-open-on)
    (let [select-holder (js/document.getElementById "select_holder")
          edit-input (js/document.getElementById "edit_input")]
      (set! (.-value edit-input) (if (= (first initial-content) \u00A0)
                                   "" ;; System provided name; clear it
                                      ;; so options appear.
                                   initial-content))
      (.add (.-classList select-holder) "active")
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
      (.remove (.-classList select-holder) "active"))))


;;; The dom the context menu is open on.
(def context-menu-open-on (atom nil))

(defn open-context-menu
  [target]
  (when (not= target @context-menu-open-on)
    (let [context-menu (js/document.getElementById "context_menu")]
      (.add (.-classList context-menu) "active")
      (scroll-to-be-visible context-menu)
      (reset! context-menu-open-on target))))

(defn close-context-menu
  "Close the context menu"
  []
  (when @context-menu-open-on
    (let [context-menu (js/document.getElementById "context_menu")]
      (reset! context-menu-open-on nil)
      (.remove (.-classList context-menu) "active"))))

(defn close-popups
  "Close the edit field, without storing the value. And close the
  context menu."
  []
  (close-edit-field)
  (close-context-menu))

;; The currently selected dom.
(def selected (atom nil))

(defn deselect []
  (let [target @selected]
    (when target
      (.remove (.-classList target) "selected")
      (reset! selected nil))))

(defn select [target]
  (when (or (not= target @selected)
            (and target (not (.contains (.-classList target) "selected"))))
    (.log js/console (str "Selecting id " (.-id target) "."))
    (deselect)
    (.add (.-classList target) "selected")
    (reset! selected target)
    (scroll-to-be-visible target)))

;; The last valid selection request id we have received from the client,
;; if we we haven't already done it, and if the user hasn't made a
;; different selection since we got it.
(def pending-server-selection-request-id (atom nil))

(defn select-and-clear-pending [target]
  (select target)
  (reset! pending-server-selection-request-id nil))

