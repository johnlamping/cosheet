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
            [cosheet.ajax :refer [request-action ajax-request]]
            [cosheet.interaction-state :refer [edit-field-open-on
                                               open-edit-field close-edit-field
                                               selected select deselect]]
            ))

(reset! components {"root" (reagent/atom [:div {:id "root" :version 0}])})

(defn is-editable? [dom]
  (and dom (.. dom -classList (contains "editable"))))

(defn descendant-with-editable
  "Given a dom, if it has editable children, return it. If a unique
  descendant  does, return it. If none do, return nil, while if more
  than one does, return false."
  [dom]
  (let [children (array-seq (.-childNodes dom))]
    (if (some is-editable? children)
      dom
      (let [candidates (filter #(not (nil? %))
                               (map descendant-with-editable children))]
        (cond (empty? candidates) nil
              (empty? (rest candidates)) (first candidates)
              true false)))))

(defn find-editable
  "Given a target and click event, return the target if it is editable,
   or the nearest child to the click event, if that child is editable."
  [target event]
  (when target
    (if (is-editable? target)
      target
      (let [holder (descendant-with-editable target)]
        (when holder
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
                        (array-seq (.-childNodes holder)))]
            (when (is-editable? closest-child)
              closest-child)))))))

(defn dom-text [target]
  (let [child (.-firstChild target)]
    (or (and child (.-nodeValue child)) "")))

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
           [:set-content (.-id target) :from old-value :to value]))))))

(defn target-being-edited? [target]
  (#{@edit-field-open-on
     (js/document.getElementById "edit_holder")
     (js/document.getElementById "edit_input")}
   target))

(defn menu-click-handler
  [event]
  (let [keyword ({"undo" :undo
                  "redo" :redo}
                 (.-id (.-target event)))
        contextual-keyword ({"add-sibling" :add-sibling
                            "add-row" :add-row
                            "add-column" :add-column}
                           (.-id (.-target event)))
        selection @selected]
    (cond keyword
          (request-action [keyword])
          (and contextual-keyword selection)
          (request-action [contextual-keyword (.-id selection)]))))

(defn click-handler
  [event]
  (let [target (.-target event)]
    (.log js/console (str "Click on id " (.-id target) "."))
    (.log js/console (str "with class " (.-className target) "."))
    (.log js/console (str "Click on " target "."))
    (when (not (target-being-edited? target))
      (store-edit-field)
      (close-edit-field)
      (if (= (.-className target) "tool")
        (menu-click-handler event)
        (let [editable (find-editable target event)]
          (when (not= editable @selected)
            (if editable
              (select editable)
              (deselect))
            (request-action [:selected (and editable (.-id editable))])))))))

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
          (deselect))
        (when (not= editable @selected)
          (request-action [:selected (and editable (.-id editable))]))))))

(defn keypress-handler
  [event]
  (let [ctrl (.-ctrlKey event)
        alt (.-altKey event)
        key-code (.-keyCode event)
        ]
    (.log js/console
          (str "keypress " (if ctrl "ctrl " "") (if alt "alt" "") key-code))
    (when (and ctrl (not alt))
      (cond  ; We can't use a case statement,
             ; as it doesn't work right with key-codes/
        (= key-codes/Z key-code) (if @edit-field-open-on
                                   (close-edit-field)
                                   (do (.log js/console "undo")
                                       (request-action [:undo])))
        (= key-codes/Y key-code)  (when (not @edit-field-open-on)
                                    (do (.log js/console "redo")
                                        (request-action [:redo])))))
    (when (and alt (not ctrl))
      (let [command (cond (= key-codes/EQUALS key-code) [:add-sibling]
                          (= key-codes/NUM_PLUS key-code) [:add-sibling] 
                          (= key-codes/PERIOD key-code) [:add-element]
                          (= key-codes/DASH key-code) [:add-row]
                          (= key-codes/R key-code) [:add-row]
                          (= key-codes/BACKSLASH key-code) [:add-column]
                          (= key-codes/C key-code) [:add-column])]
        (when (and command @selected (not @edit-field-open-on))
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
        (= key-code key-codes/BACKSPACE) (when (not @edit-field-open-on)
                                           (when @selected
                                             (.log js/console (str [:backspace]))
                                             (request-action
                                              [:delete (.-id @selected)]))
                                           ;; Prevent navigating to prev page.
                                           (.preventDefault event))
        (key-codes/isCharacterKey key-code)
        (when (and @selected (not @edit-field-open-on))
          (open-edit-field @selected (str (.-charCode event))))))))

(defn ^:export run []
  (let [app (js/document.getElementById "app")
        toolbar (js/document.getElementById "toolbar")
        edit-input (js/document.getElementById "edit_input")
        ;; The key handler makes events consistent across browsers.
        app-key-handler (gevents/KeyHandler. js/document)]
    (reagent/render [component {:id "root"}] app)
    (gevents/listen app gevents/EventType.DBLCLICK double-click-handler)
    (gevents/listen app gevents/EventType.CLICK click-handler)
    (gevents/listen app-key-handler key-handler/EventType.KEY
                    keypress-handler)
    (gevents/listen toolbar gevents/EventType.CLICK click-handler))
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


