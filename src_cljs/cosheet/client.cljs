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
                                               find-ancestor-with-class
                                               set-selector-interpretation
                                               toggle-selector-interpretation
                                               open-edit-field close-edit-field
                                               selected select deselect]]
            ))

(reset! components {"root" (reagent/atom [:div {:id "root" :version 0}])})

(defn is-editable? [dom]
  (when (and dom (exists? dom))
    (let [classes (.-classList dom)]
      (when (and classes (exists? classes))
        (.contains classes "editable")))))

(defn is-immutable? [dom]
  (when (and dom (exists? dom))
    (let [classes (.-classList dom)]
      (when (and classes (exists? classes))
        (.contains classes "immutable")))))

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

(defn target-in-edit-holder? [target]
  (find-ancestor-with-class target "edit_holder" 4))

(defn open-expand-popup
  "Open the expand popup window.
  This should be called while in any event handler that will end up opening
  up the window, so we are less likely to be stopped by a popup blocker
  and more likely to get a popup, rather than a new tab."
  []
  (.open
   js/window "" "CosheetExpandPopup",
   (str "width=600,height=600,left=150,top=100,centerscreen=yes,toolbar=yes")))

(defn menu-click-handler
  [logical-target]
  (let [id (.-id logical-target)
        local-command ({"narrow_selector_interpretation"
                        [set-selector-interpretation :narrow]
                        "broad_selector_interpretation"
                        [set-selector-interpretation :broad]}
                       id)
        keyword ({"undo" :undo
                  "redo" :redo}
                 id)
        contextual-keyword ({"expand" :expand
                             "add-element" :add-element
                             "add-twin" :add-twin
                             "add-sibling" :add-sibling
                             "add-row" :add-row
                             "add-column" :add-column}
                           id)
        selection @selected]
    (.log js/console (str "menu click" id))
    (cond local-command
          (apply (first local-command) (rest local-command))
          keyword
          (request-action [keyword])
          (and contextual-keyword selection)
          (do (when (= contextual-keyword :expand) (open-expand-popup))
              (request-action [contextual-keyword (.-id selection)])))))

(defn alternate-interpretation-click-handler
  [event]
  (toggle-selector-interpretation)
  (request-action [:alternate]))

(defn click-handler
  [event]
  (let [target (.-target event)]
    (.log js/console (str "Click on id " (.-id target) "."))
    (.log js/console (str "with class " (.-className target) "."))
    (.log js/console (str "Click on " target "."))
    (let [in-edit-holder (target-in-edit-holder? target)]
      (when (not in-edit-holder)
        (store-edit-field)
        (close-edit-field))
      (if-let [tool-target (find-ancestor-with-class target "tool" 1)]
        (menu-click-handler tool-target)
        (when (not in-edit-holder)
          (let [editable (find-editable target event)]
            (when (not= editable @selected)
              (if editable
                (select editable)
                (deselect))
              (request-action [:selected (and editable (.-id editable))]))))))))

(defn double-click-handler
  [event]
  (let [target (.-target event)]
    (.log js/console (str "Double click on id " (.-id target) "."))
    (.log js/console (str "with class " (.-className target) "."))
    (.log js/console (str "Double click on " target "."))
    (when (not (target-in-edit-holder? target))
      (store-edit-field)
      (close-edit-field)
      (let [editable (find-editable target event)]
        (if editable
          (do (select editable)
              (when (not (is-immutable? editable))
                (open-edit-field editable (dom-text editable))))
          (deselect))
        (when (not= editable @selected)
          (request-action [:selected (and editable (.-id editable))]))))))

(defn keypress-handler
  [event]
  (let [ctrl (.-ctrlKey event)
        meta (.-metaKey event)
        alt (.-altKey event)
        key-code (or (.-key event) (.-keyCode event))
        ]
    (.log js/console
          (str "keydown "
               (if ctrl "ctrl " "") (if alt "alt " "") (if meta "meta " "")
               key-code))
    (when (= (count (filter identity [ctrl alt meta])) 1)
      (cond  ; We can't use a case statement,
             ; as it doesn't work right with key-codes.
        (= key-codes/Z key-code) (do (.preventDefault event)
                                     (if @edit-field-open-on
                                       (close-edit-field)
                                       (do (.log js/console "undo")
                                           (request-action [:undo]))))
        (= key-codes/Y key-code) (do (.preventDefault event)
                                     (when (not @edit-field-open-on)
                                       (do (.log js/console "redo")
                                           (request-action [:redo]))))))
    (when (and alt (not (or ctrl meta)))
      (let [command (cond (= key-codes/EQUALS key-code) [:add-twin]
                          (= key-codes/NUM_PLUS key-code) [:add-twin] 
                          (= key-codes/PERIOD key-code) [:add-element]
                          (= key-codes/S key-code) [:add-sibling]
                          (= key-codes/HASH key-code) [:add-sibling]
                          (= key-codes/DASH key-code) [:add-row]
                          (= key-codes/R key-code) [:add-row]
                          (= key-codes/BACKSLASH key-code) [:add-column]
                          (= key-codes/C key-code) [:add-column]
                          (= key-codes/E key-code) [:expand])]
        (when (and command @selected (not @edit-field-open-on))
          (.log js/console (str command))
          (when (= (first command) :expand) (open-expand-popup))
          (request-action
           (apply vector (first command) (.-id @selected) (rest command))))))
    (when (not (or ctrl alt meta))
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
          (when (and @selected
                     (not (is-immutable? @selected))
                     (not @edit-field-open-on))
            (open-edit-field @selected (str (.-charCode event))))))))

(defn ^:export run []
  (let [app (js/document.getElementById "app")
        toolbar (js/document.getElementById "toolbar")
        edit-input (js/document.getElementById "edit_input")
        ;; The key handler makes events consistent across browsers.
        ;; TODO: We no longer use this because it seems to rely on
        ;; the deprecated field KeyboardEvent.keyIdentifier. See if we can
        ;; get a more recent version of goog.events that fixes the problem.
        ;; app-key-handler (gevents/KeyHandler. js/document)
        ]
    (reagent/render [component {:id "root"}] app)
    (gevents/listen app gevents/EventType.DBLCLICK double-click-handler)
    (gevents/listen app gevents/EventType.CLICK click-handler)
    (gevents/listen (js/document.getElementById "alternate_interpretation")
                    gevents/EventType.CLICK
                    alternate-interpretation-click-handler)    
    (gevents/listen js/document gevents/EventType.KEYDOWN keypress-handler)
    ;(gevents/listen app-key-handler key-handler/EventType.KEY keypress-handler)
    (gevents/listen toolbar gevents/EventType.CLICK click-handler)
    (set-selector-interpretation :broad))
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
