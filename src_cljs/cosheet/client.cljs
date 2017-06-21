(ns cosheet.client
  (:require [reagent.core :as reagent]
            [goog.events :as gevents]
            [goog.events.KeyCodes :as key-codes]
            [goog.events.KeyHandler :as key-handler]
            ;; Note: We seem to have to declare any closure packages used
            ;; by our libraries in order for them to be visible to
            ;; Chrome.
            [cosheet.client-utils :refer [component components
                                          add-pending-clean]]
            [cosheet.dom-utils :refer [is-editable? is-immutable?
                                       descendant-with-editable find-editable
                                       dom-text find-ancestor-with-class
                                       next-mutable-editable]]
            cosheet.hiccup-utils
            [cosheet.ajax :refer [request-action request-replay
                                  ajax-request ajax-if-pending]]
            [cosheet.interaction-state :refer [edit-field-open-on
                                               set-selector-interpretation
                                               toggle-selector-interpretation
                                               open-edit-field close-edit-field
                                               selected select deselect]]
            ))

(reset! components {"root" (reagent/atom [:div {:id "root" :version 0}])})

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

(defn target-in-select-holder? [target]
  (find-ancestor-with-class target "select_holder" 4))

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
                             "add-twin" :add-twin
                             "add-element" :add-element
                             "add-label" :add-label
                             "add-sibling" :add-sibling
                             "add-row" :add-row
                             "add-column" :add-column
                             "delete-column" :delete-column
                             "delete-row" :delete-row}
                           id)
        selection @selected]
    (.log js/console (str "menu click " id))
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
  (let [target (.-target event)
        ;; If a cell is selected, but not being edited, the select holder
        ;; is in front of it, but empty. Move the click to the cell.
        effective-target (if (= (.-id target) "select_holder")
                           @selected target)]
    (.log js/console (str "Click on id " (.-id target) "."))
    (.log js/console (str "with class " (.-className target) "."))
    (let [in-select-holder (target-in-select-holder? effective-target)]
      (when (not in-select-holder)
        (store-edit-field)
        (close-edit-field))
      (if-let [tool-target (find-ancestor-with-class effective-target "tool" 1)]
        (do (when @edit-field-open-on
              ;; A click on the tool can cause a loss of focus. Put it back.
              (.focus (js/document.getElementById "edit_input")))
            (menu-click-handler tool-target))
        (when (not in-select-holder)
          (let [editable (find-editable effective-target event)]
            (when (not= editable @selected)
              (if editable
                (select editable)
                (deselect))
              (request-action [:selected (and editable (.-id editable))]))))))))

(defn double-click-handler
  [event]
  (let [target (.-target event)
        ;; If a cell is selected, but not being edited, the select holder
        ;; is in front of it, but empty. Move the click to the cell.
        effective-target (if (= (.-id target) "select_holder")
                           @selected target)]
    (.log js/console (str "Double click on id " (.-id target) "."))
    (.log js/console (str "with class " (.-className target) "."))
    (when (not (target-in-select-holder? effective-target))
      (store-edit-field)
      (close-edit-field)
      (let [editable (find-editable effective-target event)]
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
        total-shift (count (filter identity [ctrl alt meta]))]
    (.log js/console
          (str "keydown "
               (if ctrl "ctrl " "") (if alt "alt " "") (if meta "meta " "")
               key-code))
    (when 
      (cond
        ))
    (when (= total-shift 1)
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
    (when (and alt meta)
      (when (= key-codes/R key-code)
        (request-replay :all)))
    (when (and ctrl (not alt) (not meta))
      (let [command (cond (= key-codes/EQUALS key-code) [:add-twin]
                          (= key-codes/NUM_PLUS key-code) [:add-twin] 
                          (= key-codes/PERIOD key-code) [:add-element]
                          (= key-codes/L key-code) [:add-label]
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
    (when (= total-shift 0)
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
                                             (.log js/console
                                                   (str [:backspace]))
                                             (request-action
                                              [:delete (.-id @selected)]))
                                           ;; Prevent navigating to prev page.
                                           (.preventDefault event))
        (= key-codes/TAB key-code)
        (do (.preventDefault event)
            (when @edit-field-open-on
              (store-edit-field)
              (close-edit-field))
            (when-let [selection @selected]
              (when (not (find-ancestor-with-class selection "tabs-holder"))
                (when-let [next (next-mutable-editable selection)]
                  (select next)))))
          (key-codes/isCharacterKey key-code)
          (when (and @selected
                     (not (is-immutable? @selected))
                     (not @edit-field-open-on))
            (open-edit-field @selected (str (.-charCode event))))))))

(defn unload-handler
  [event]
  (.log js/console "Unloading.")
  (ajax-request {:unload true} 100)
  nil)

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
    (gevents/listen js/window gevents/EventType.UNLOAD unload-handler)
    (set-selector-interpretation :narrow))
  (add-pending-clean js/window.location.href)
  (ajax-if-pending)) 

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
