(ns cosheet2.client
  (:require reagent.core 
            reagent.dom
            ;; Note: We seem to have to declare any closure packages used
            ;; by our libraries in order for them to be visible to
            ;; Chrome.
            [cosheet2.client-utils :refer [component components
                                           add-pending-clean]]
            [cosheet2.dom-utils :refer [is-editable? is-immutable?
                                        descendant-with-editable find-editable
                                        dom-text find-ancestor-with-class
                                        next-mutable-editable]]
            cosheet2.hiccup-utils
            [cosheet2.ajax :refer [timed-log request-action request-replay
                                   ajax-request ajax-if-pending]]
            [cosheet2.interaction-state :refer [edit-field-open-on
                                                open-edit-field close-edit-field
                                                selected deselect
                                                select-and-clear-pending]]
            ))

(reset! components {"root" (reagent.core/atom [:div {:id "root" :version 0}])})

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
        command-name (if (or (clojure.string.ends-with? id "-tool")
                             (clojure.string.ends-with? id "-menu"))
                       (subs id 0 (- (count id) 5))
                       id)
        keyword (case command-name
                  "undo" :undo
                  "redo" :redo
                  "quit-batch-edit" :quit-batch-edit
                  nil)
        contextual-keyword (case command-name
                             "expand" :expand
                             "add-twin" :add-twin
                             "add-element" :add-element
                             "add-label" :add-label
                             "add-sibling" :add-sibling
                             "add-row" :add-row
                             "add-column" :add-column
                             "delete-column" :delete-column
                             "delete-row" :delete-row
                             "batch-edit" :batch-edit
                             nil)
        selection @selected]
    (.log js/console (str "menu click " id))
    (cond keyword
          (request-action [keyword])
          (and contextual-keyword
               (or selection (= contextual-keyword :batch-edit)))
          (do (when (= contextual-keyword :expand) (open-expand-popup))
              (request-action [contextual-keyword
                               (when selection (.-id selection))])))))

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
                (select-and-clear-pending editable)
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
          (do (select-and-clear-pending editable)
              (when (not (is-immutable? editable))
                (open-edit-field editable (dom-text editable))))
          (deselect))
        (when (not= editable @selected)
          (request-action [:selected (and editable (.-id editable))]))))))

(defn context-menu-handler
  [event]
  (let [target (.-target event)]
    ;; TODO: !!! Turn this on once we are bringing up a menu.
    ;(.preventDefault event) ;; Stop the browser menu from appearing
    ))

(defn keydown-handler
  [event]
  (let [ctrl (.-ctrlKey event)
        meta (.-metaKey event)
        alt (.-altKey event)
        key (.-key event)
        total-shift (count (filter identity [ctrl alt meta]))]
    (.log js/console
          (str "keydown "
               (if ctrl "ctrl " "") (if alt "alt " "") (if meta "meta " "")
               (str" key " key)))
    (.log js/console
          (str (if @selected "selected " "")
               (if @edit-field-open-on "edit field open " "")
               (if (is-immutable? @selected) "immutable " "")))
    (when (= total-shift 1)
      (case key
        "z" (do (.preventDefault event)
                (if @edit-field-open-on
                  (close-edit-field)
                  (do (.log js/console "undo")
                      (request-action [:undo]))))
        "y" (do (.preventDefault event)
                (when (not @edit-field-open-on)
                  (do (.log js/console "redo")
                      (request-action [:redo]))))
        "q" (do (.preventDefault event)
                (.log js/console "quit-batch-edit")
                (close-edit-field)
                (request-action [:quit-batch-edit]))
        nil))
    (when (and alt meta)
      (when (= "r" key)
        (request-replay :all)))
    (when (and ctrl (not alt) (not meta))
      (let [command (case key
                      ("=" "+") [:add-twin] 
                      "." [:add-element]
                      "l" [:add-label]
                      ("s" "#") [:add-sibling]
                      ("-" "_" "r") [:add-row]
                      ("\\" "|" "c") [:add-column]
                      "e" [:expand]
                      "b" [:batch-edit]
                      nil)
            id (when @selected (.-id @selected))]
        (when (and command (not @edit-field-open-on)
                   (or id (#{:batch-edit :quit-batch-edit} (first command))))
          (.log js/console (str command))
          (when (= (first command) :expand) (open-expand-popup))
          (request-action
           (apply vector (first command) id (rest command))))))
    (when (= total-shift 0)
      (if (= (count key) 1)
        ;; The key generates a character in a text field. Open up the
        ;; edit field if necessary.
        (when (and @selected
                   (not (is-immutable? @selected))
                   (not @edit-field-open-on))
          (open-edit-field @selected (str (.-charCode event)))))
      (case key
        "Escape" (close-edit-field)
        "Enter" (do (store-edit-field)
                    (close-edit-field))
        "Delete" (when (and @selected
                            (not @edit-field-open-on))
                   (.log js/console (str [:delete]))
                   (request-action
                    [:delete (.-id @selected)]))
        "Backspace" (when (not @edit-field-open-on)
                      ;; Prevent navigating to prev page.
                      (.preventDefault event)
                      (when @selected
                        (.log js/console
                              (str [:backspace]))
                        (request-action
                         [:delete (.-id @selected)])))
        "Tab" (do (.preventDefault event)
                  (when @edit-field-open-on
                    (store-edit-field)
                    (close-edit-field))
                  (when-let [selection @selected]
                    (when (not (find-ancestor-with-class
                                selection "tabs-holder"))
                      (when-let [next (next-mutable-editable selection)]
                        (select-and-clear-pending next)))))
        nil))))

(defn unload-handler
  [event]
  (.log js/console "Unloading.")
  (ajax-request {:unload true} 100)
  nil)

(defn ^:export run []
  (let [app (js/document.getElementById "app")
        toolbar (js/document.getElementById "toolbar")
        edit-input (js/document.getElementById "edit_input")]
    (reagent.dom/render [component {:id "root"}] app)
    (.addEventListener app "click" click-handler)
    (.addEventListener app "dblclick" double-click-handler)
    (.addEventListener app "contextmenu" context-menu-handler)
    (.addEventListener js/document "keydown" keydown-handler)
    (.addEventListener toolbar "click" click-handler)
    (.addEventListener js/window "unload" unload-handler))
  (timed-log "page loaded.")
  (add-pending-clean js/window.location.href)
  (ajax-if-pending)) 

