(ns cosheet.interaction-state
  (:require [goog.dom :as gdom]
            [cosheet.dom-utils :refer [find-ancestor-with-class
                                       scroll-to-be-visible]]))

;;; These are the UI operations on the edit field, alternate interpretation
;;; field, and on selections. We
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
      (when-let [table-main (find-ancestor-with-class target "table-main")]
        (scroll-to-be-visible select-holder table-main))
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

(defn set-alternate-field
  [text]
  (let [alternate-holder (js/document.getElementById
                          "alternate_interpretation_holder")]
    (if (nil? text)
      (set! (.-className alternate-holder) "")
      (let [alternate (js/document.getElementById "alternate_interpretation")]
        (set! (.-textContent (.-firstChild alternate-holder)) (first text))
        (set! (.-textContent alternate) (second text))
        (set! (.-textContent (.-lastChild alternate-holder))
              (or (second (rest text)) " "))
        (set! (.-className alternate-holder) "visible")))))

;;; :broad or :narrow
(def selector-interpretation (atom nil))

(def selected (atom nil)) ;; The currently selected dom.

;;; The selector-scope dom of the currently selected dom if it is a selector
;;; and we are doing the narrow interpretation of selection scopes.
;;; That dom will have the class "narrow-scope"
(def narrow-selector-scope (atom nil))

;;; The selector-scope dom of the currently selected dom if it is a selector
;;; and we are doing the broad interpretation of selection scopes.
;;; That dom will have the class "broad-scope"
(def broad-selector-scope (atom nil))

;;; The dom of the header of the currently selected column if we are doing
;;; the broad interpretation of selection scopes and have a column selected.
;;; That dom will have the class "broad-column"
(def broad-selection-column (atom nil))

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

(defn set-selection-classes
  "Set the appropriate dom classes, if any, to reflect the current selection."
  []
  (let [selection @selected
        selector-scope (if-let [selectors (when selection
                                            (find-ancestor-with-class
                                             selection "selectors"))]
                         (find-ancestor-with-class selectors "selector-scope"))
        interpretation @selector-interpretation
        narrow-scope (when (= interpretation :narrow) selector-scope)
        broad-scope (when (= interpretation :broad) selector-scope)
        broad-column (and selection
                          (= interpretation :broad)
                          (find-ancestor-with-class selection "column-header"))]
    (set-special-class
     narrow-scope narrow-selector-scope "narrow-scope")
    (set-special-class
     broad-scope broad-selector-scope "broad-scope")
    (set-special-class
     broad-column broad-selection-column "broad-column")))

(defn interpretation-selector-dom
  "Return the dom choice button for the given interpretation of selectors."
  [interpretation]
  (js/document.getElementById ({:narrow "narrow_selector_interpretation"
                                :broad "broad_selector_interpretation"}
                               interpretation)))

(defn opposite-selector-interpretation
  [interpretation]
  ({:broad :narrow
    :narrow :broad} interpretation))

(defn set-selector-interpretation
  [interpretation]
  (when (not= interpretation @selector-interpretation)
    (.log js/console
          (str "setting selector interpretation " interpretation "."))
    (reset! selector-interpretation interpretation)
    (.add (.-classList (interpretation-selector-dom interpretation))
          "picked")
    (.remove (.-classList (interpretation-selector-dom
                           (opposite-selector-interpretation interpretation)))
             "picked")
    (set-selection-classes)))

(defn toggle-selector-interpretation
  []
  (set-selector-interpretation
   (opposite-selector-interpretation @selector-interpretation))
  (set-selection-classes))

;;; The last version of broad-selector-scope that was the result of a user
;;; selection action. This is used only by adjust-selection-interpretation.
;;; We can have deselections for various random reasons,
;;; like doms changing. Those shouldn't affect the current selection
;;; interpretation.
(def last-chosen-broad-selector-scope (atom nil))

(defn adjust-selection-interpretation
  "Change the selection interpretation to narrow if it was broad and we are
  now in a different selector scope."
  [new-selection]
  (let [selector-scope (when-let [selectors (when new-selection
                                              (find-ancestor-with-class
                                               new-selection "selectors"))]
                         (find-ancestor-with-class
                          selectors "selector-scope"))]
    (when (not= selector-scope @last-chosen-broad-selector-scope)
      (set-selector-interpretation :narrow)
      (reset! last-chosen-broad-selector-scope selector-scope))))

(defn deselect []
  (let [target @selected]
    (when target
      (.remove (.-classList target) "selected")
      ;; Put it at the end, where it will be invisible, but still findable.
      (gdom/appendChild  (js/document.getElementById "app")
                         (js/document.getElementById "select_holder"))
      (reset! selected nil)
      (set-selection-classes))))

(defn select [target]
  (.log js/console (str "Selecting id " (.-id target) "."))
  (.log js/console (str "current selection " @selected))
  (when (not= target @selected)
    (adjust-selection-interpretation target)
    (deselect)
    (.add (.-classList target) "selected")
    (gdom/appendChild target (js/document.getElementById "select_holder"))
    (.log js/console (str "Selected id " (.-id target) "."))
    (reset! selected target)
    (set-selection-classes)
    (scroll-to-be-visible target)))

