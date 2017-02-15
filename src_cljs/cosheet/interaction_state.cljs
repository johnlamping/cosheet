(ns cosheet.interaction-state
  (:require [goog.dom :as gdom]))

(defn has-class?
  [node class-name]
  (let [classes (.-classList node)]
    (when (exists? classes)
      (.contains classes class-name))))

(defn find-ancestor-with-class
  "Return the first ancestor with the given class,
  not going above max-depth ancestors (if present)"
  [node class-name & [max-depth]]
  (if (has-class? node class-name)
    node
    (when (not= max-depth 0)
      (let [parent (.-parentNode node)]
        (when (and parent (exists? parent))
          (find-ancestor-with-class
           parent class-name (when max-depth (dec max-depth))))))))

(defn offset-parent-below-ancestor
  "Return the offset parent of the node, as long as it is at or below
  the ancestor."
  [node ancestor]
  (when-let [parent (.-offsetParent node)]
    (loop [node (.-parentNode node)]
      (if (= node parent)
        node
        (when (and node (exists? node) (not= node ancestor))
          (recur (.-parentNode node)))))))

(defn left-offset-in-ancestor
  "Return the left offset of the node with respect to
   the given ancestor."
  [node ancestor]
  (if (= node ancestor)
    0
    (let [offset-parent (offset-parent-below-ancestor node ancestor)]
      (if offset-parent
        (+ (.-offsetLeft node)
           (left-offset-in-ancestor offset-parent ancestor))
        0))))

(defn scroll-horizontally-to-be-visible
  "Horizontally scroll the node to be fully visible, assuming that the ancestor
   is the node with the scrolling content."
  [node ancestor]
  (let [left (left-offset-in-ancestor node ancestor) 
        right (+ left (.-offsetWidth node) 3)
        available (.-clientWidth ancestor)
        current (.-scrollLeft ancestor)]
    (if (> (- right current) available)
      (set! (.-scrollLeft ancestor) (max 0 (- right available)))
      (if (< left current)
        (set! (.-scrollLeft ancestor) left)))))

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
        (scroll-horizontally-to-be-visible select-holder table-main))
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
(def selector-interpretation (atom :broad))

(def selected (atom nil)) ;; The currently selected dom.

;;; The selector-scope dom of the currently selected dom if it is a selector
;;; and we are doing the narrow interpretation of selection scopes.
;;; That dom will have the class "narrow-selection"
(def narrow-selector-scope (atom nil))

;;; The dom of the header of the currently selected column if we are doing
;;; the broad interpretation of selection scopes.
;;; That dom will have the class "wide-selection"
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
        narrow-scope (when (and selection
                                (= @selector-interpretation :narrow))
                       (if-let [selectors (find-ancestor-with-class
                                           selection "selectors")]
                         (find-ancestor-with-class selectors "selector-scope")))
        broad-column (when (and selection
                                (= @selector-interpretation :broad))
                       (find-ancestor-with-class selection "column-header"))]
    (set-special-class
     narrow-scope narrow-selector-scope "narrow-interpretation")
    (set-special-class
     broad-column broad-selection-column "broad-interpretation")))

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
  (set-selection-classes))

(defn toggle-selector-interpretation
  []
  (set-selector-interpretation
   (opposite-selector-interpretation @selector-interpretation)))

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
    (deselect)
    (.add (.-classList target) "selected")
    (gdom/appendChild target (js/document.getElementById "select_holder"))
    (.log js/console (str "Selected id " (.-id target) "."))
    (reset! selected target)
    (set-selection-classes)))

