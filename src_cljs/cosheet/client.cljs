(ns cosheet.client
  (:require [reagent.core :as reagent]
            [goog.dom :as gdom]
            [goog.events :as gevents]
            [goog.events.KeyCodes :as key-codes]
            [goog.events.KeyHandler :as key-handler]
            ;; Note: We seem to have to declare any closure packages used
            ;; by our libraries in order for them to be visible to
            ;; Chrome.
            [cosheet.client-utils :refer
             [component components add-pending-action]]
            cosheet.dom-utils
            [cosheet.ajax :refer [ajax-if-pending ajax-request]]
            [cosheet.edit-field :refer [edit-field-open-on
                                        open-edit-field close-edit-field]]
            ))

(reset! components {"root" (reagent/atom [:div {:id "root" :version 0}])})

(def selected (atom nil)) ;; The currently selected dom.

(defn is-editable? [dom]
  (and dom (.. dom -classList (contains "editable"))))

(defn find-editable
  "Given a target and click event, return the target if it is editable,
   or the nearest child to the click event, if that child is editable."
  [target event]
  (when target
    (if (is-editable? target)
      target
      (let [x (.-clientX event)
            y (.-clientY event)
            [closest-child _]
            (reduce (fn [[closest best-distance] child]
                      (let [rect (.getBoundingClientRect child)
                            dist (+ (max 0 (- (.-left rect) x))
                                    (max 0 (- x (.-right rect)))
                                    (max 0 (- (.-top rect) y))
                                    (max 0 (- y (.-bottom rect))))]
                        (if (< dist best-distance)
                          [child dist]
                          [closest best-distance])))
                    [nil 1e10]
                    (array-seq (.-childNodes target)))]
        (when (is-editable? closest-child)
          closest-child)))))

(defn deselect []
  (let [target @selected]
    (when target
    (.remove (.-classList target) "selected")
    (reset! selected nil))))

(defn select [target]
  (.log js/console (str "Selecting id " (.-id target) "."))
  (.log js/console (str "current selection " @selected))
  (when (not= target @selected)
    (deselect)
    (.add (.-classList target) "selected")
    (.log js/console (str "Selected id " (.-id target) "."))
    (reset! selected target)))

(defn dom-text [target]
  (let [child (.-firstChild target)]
    (or (and child (.-nodeValue child)) "")))

(defn request-action
  [action]
  (add-pending-action action)
  (ajax-if-pending))

(defn store-edit-field
  []
  (let [target @edit-field-open-on]
    (when target
      (let [edit-input (js/document.getElementById "edit_input")
            value (.-value edit-input)
            old-value (dom-text target)]
        (when (not= value old-value)
          (.log js/console (str "storing " value
                                " into " (.-id target)))
          (request-action
           [:set-content (.-id target) old-value value]))))))

(defn target-being-edited? [target]
  (#{@edit-field-open-on
     (js/document.getElementById "edit_holder")
     (js/document.getElementById "edit_input")}
   target))

(defn click-handler
  [event]
  (let [target (.-target event)]
    (.log js/console (str "Click on id " (.-id target) "."))
    (.log js/console (str "with class " (.-className target) "."))
    (.log js/console (str "Click on " target "."))
    (when (not (target-being-edited? target))
      (store-edit-field)
      (close-edit-field)
      (let [editable (find-editable target event)]
        (if editable
          (select (find-editable target event))
          (deselect))))))

(defn double-click-handler
  [event]
  (let [target (.-target event)]
    (.log js/console (str "Double click on id " (.-id target) "."))
    (.log js/console (str "with class " (.-className target) "."))
    (.log js/console (str "Double click on " target "."))
    (when (not (target-being-edited? target))
      (store-edit-field)
      (close-edit-field)
      (let [editable (find-editable target event)]
        (if editable
          (do (select editable)
              (open-edit-field editable (dom-text editable)))
          (deselect))))))

(defn keypress-handler
  [event]
  (let [ctrl (.-ctrlKey event)
        alt (.-altKey event)
        key-code (.-keyCode event)
        ]
    (.log js/console
          (str "keypress " (if ctrl "ctrl " "") (if alt "alt" "") key-code))
    (when (and ctrl (not alt))
      (cond  ; We can't use case, as it doesn't work right with key-codes/
        ;; TODO: If the edit field is not open, make this
        ;; undo the last action.
        (= key-code key-codes/Z)
        (when @edit-field-open-on (close-edit-field))))
    (when (and alt (not ctrl))
      (let [command (cond (= key-codes/EQUALS key-code) [:add-sibling :after]
                          (= key-codes/NUM_PLUS key-code) [:add-sibling :after] 
                          (= key-codes/PERIOD key-code) [:add-element]
                          (= key-codes/DASH key-code) [:add-row :after]
                          (= key-codes/R key-code) [:add-row :after])]
        (when (and command @selected)
          (.log js/console (str command))
          (request-action
           (apply vector (first command) (.-id @selected) (rest command))))))
    (when (not (or ctrl alt))
      (cond
        (= key-code key-codes/ESC) (close-edit-field)
        (= key-code key-codes/ENTER) (do (store-edit-field)
                                         (close-edit-field))
        (= key-code key-codes/DELETE) (when (and @selected
                                                 (not @edit-field-open-on))
                                        (.log js/console (str [:delete]))
                                        (request-action
                                         [:delete (.-id @selected)]))
        (key-codes/isCharacterKey key-code)
        (when (and @selected (not @edit-field-open-on))
          (open-edit-field @selected (str (.-charCode event))))))))

(defn ^:export run []
  (let [app (js/document.getElementById "app")
        edit-input (js/document.getElementById "edit_input")
        ;; The key handler makes events consistent across browsers.
        app-key-handler (gevents/KeyHandler. js/document)]
    (reagent/render [component {:id "root"}] app)
    (gevents/listen app gevents/EventType.DBLCLICK double-click-handler)
    (gevents/listen app gevents/EventType.CLICK click-handler)
    (gevents/listen app-key-handler key-handler/EventType.KEY
                    keypress-handler))
  (ajax-request {:initialize true}))

;;; TODO: Get rid of this eventually; It's just something cute.
(comment
  (defonce time-updater
    (js/setInterval
     #(let [clock (@components :clock)]
        (when clock
          (let [now
                (-> (js/Date.) .toTimeString (clojure.string/split " ")  first)]
            (swap! clock (fn [old] (assoc old 2 now))))))
     1000)))


