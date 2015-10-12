(ns cosheet.edit-field
  (:require [goog.dom :as gdom]))

;;; These are the UI operations on the edit field. We put them in
;;; their own file so both client.cljs and ajax.cljs can access them.

(def edit-field-open-on (atom nil)) ;; The dom the edit field is open on.

(defn open-edit-field [target initial-content]
  (when (not= target @edit-field-open-on)
    (let [edit-holder (js/document.getElementById "edit_holder")
          edit-input (js/document.getElementById "edit_input")]
      (set! (.-value edit-input) initial-content)
      (gdom/appendChild target edit-holder)
      (.focus edit-input)
      (.select edit-input)
      (reset! edit-field-open-on target))))

(defn close-edit-field
  "Close the edit field, without storing the value."
  []
  (when @edit-field-open-on
    (let [edit-holder (js/document.getElementById "edit_holder")
          app (js/document.getElementById "app")]
      (reset! edit-field-open-on nil)
      ;; Put it at the end, where it will be invisible, but still findable.
      (gdom/appendChild app edit-holder))))

