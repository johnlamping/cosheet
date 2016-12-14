(ns cosheet.interaction-state
  (:require [goog.dom :as gdom]))

(defn find-ancestor-with-class
  "Return the first ancestor with the given class,
  not going above max-depth ancestors (if present)"
  [node class-name & [max-depth]]
  (if (let [classes (.-classList node)]
        (when (exists? classes)
          (.contains classes class-name)))
    node
    (when (not= max-depth 0)
      (let [parent (.-parentNode node)]
        (when (and parent (exists? parent))
          (find-ancestor-with-class
           parent class-name (when max-depth (dec max-depth))))))))

;;; These are the UI operations on the edit field, alternate interpretation
;;; field, and on selections. We
;;; put them in their own file so both client.cljs and ajax.cljs can
;;; access them.

(def edit-field-open-on (atom nil)) ;; The dom the edit field is open on.

(defn open-edit-field
  [target initial-content]
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

(def selected (atom nil)) ;; The currently selected dom.

;;; The selector-scope dom of the currently selected dom if it is a selector
;;; and we are doing the narrow interpretation of selection scopes.
;;; That dom will have the class "narrow-selection"
(def narrow-selector-scope (atom nil))

;;; :broad or :narrow
(def selector-interpretation (atom :broad))

(defn set-selector-scope
  "Set the appropriate dom, if any, to reflect the current selector scope."
  []
  (let [selection @selected
        selectors (when selection
                    (find-ancestor-with-class selection "selectors"))
        new-scope (when (and selection
                             selectors
                             (= @selector-interpretation :narrow))
                    (find-ancestor-with-class selectors "selector-scope"))
        current-scope @narrow-selector-scope]
    (when (not= new-scope current-scope)
      (when current-scope
        (.remove (.-classList current-scope) "narrow-interpretation"))
      (when new-scope
        (.add (.-classList new-scope) "narrow-interpretation"))
      (reset! narrow-selector-scope new-scope))))

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
  (.log js/console (str "setting selector interpretation " interpretation "."))
  (reset! selector-interpretation interpretation)
  (.add (.-classList (interpretation-selector-dom interpretation))
        "picked")
  (.remove (.-classList (interpretation-selector-dom
                         (opposite-selector-interpretation interpretation)))
           "picked")
  (set-selector-scope))

(defn toggle-selector-interpretation
  []
  (set-selector-interpretation
   (opposite-selector-interpretation @selector-interpretation)))

(defn deselect []
  (let [target @selected]
    (when target
      (.remove (.-classList target) "selected")
      (reset! selected nil)
      (set-selector-scope))))

(defn select [target]
  (.log js/console (str "Selecting id " (.-id target) "."))
  (.log js/console (str "current selection " @selected))
  (when (not= target @selected)
    (deselect)
    (.add (.-classList target) "selected")
    (.log js/console (str "Selected id " (.-id target) "."))
    (reset! selected target)
    (set-selector-scope)))

