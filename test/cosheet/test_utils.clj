(ns cosheet.test-utils
  (require [clojure.test :refer [assert-expr do-report]]
           (cosheet [entity :refer [content elements atom?
                                    mutable-entity? description->entity]]
                    [entity-impl :as entity-impl]
                    [store :refer [new-element-store new-mutable-store
                                   current-store update-content!]]
                    store-impl
                    mutable-store-impl
                    [expression-manager :as expression-manager
                     :refer [current-value]]
                    [debug :refer [simplify-for-print]]
                    [reporter :refer [value reporter?]]
                    [store-utils :refer [add-entity]])))

(def differences)

(defn special-form? [pattern]
  (and (sequential? pattern) (= ::test (first pattern))))

(defn report-difference
  [value pattern]
  [:!= value pattern])

(defn sequence-differences [value pattern]
  (let [vcount (count value)
        pcount (count pattern)
        padded-value (cond-> value (< vcount pcount)
                             (concat (repeat (- pcount vcount) ::nothing)))
        padded-pattern (cond-> pattern (< pcount vcount)
                               (concat (repeat (- vcount pcount) ::nothing)))
        diffs (map differences padded-value padded-pattern)]
    (when (not-every? nil? diffs)
      (if (every? (fn [diff] (and (sequential? diff) (= :!= (first diff))))
                  diffs)
        (report-difference value pattern)
        (drop-last (count (take-while nil? (reverse diffs))) diffs)))))

(defn set-differences [value pattern]
  (let [unmatched-values (clojure.set/difference value pattern)
        unmatched-patterns (clojure.set/difference pattern value)]
    ;; The remainders might still match via differences; try all combinations.
    (let [[unmatched-values unmatched-patterns]
          (reduce
           ;; In this reduce, unmatched values will start empty, and grow,
           ;; while unmatched-patterns starts at all of them, and shrinks.
           (fn [[unmatched-values unmatched-patterns] value]
             (let [match (some #(and (nil? (differences value %)) %)
                               unmatched-patterns)]
               (if match
                 [unmatched-values (disj unmatched-patterns match)]
                 [(conj unmatched-values value) unmatched-patterns])))
           [#{} unmatched-patterns] unmatched-values)]
      (when (or (not (empty? unmatched-values))
                (not (empty? unmatched-patterns)))
        (report-difference unmatched-values unmatched-patterns)))))

(defn map-differences [value pattern]
  (let [all-keys (clojure.set/union (set (keys value)) (set (keys pattern)))]
    (let [errors (reduce
                  (fn [errors key]
                    (let [error (differences (get value key ::nothing)
                                             (get pattern key ::nothing))]
                      (cond-> errors error (assoc key error))))
                  {} all-keys)]
      (when (not (empty? errors))
        (if (empty? (clojure.set/intersection
                     (set (keys value)) (set (keys pattern))))
          (report-difference value pattern)
          errors)))))

(def matchers {sequential? sequence-differences
               map? map-differences
               set? set-differences})

(defn differences
  "Check that the value matches the pattern, returning nil if it matches,
   and a subpart of the value if it doesn't, showing each location with
   a difference.
   A pattern can be a sequence, a map, or a set, or it can be the
   special form [::test fn & args, which will cause the function
   to be called with the value and the arguments, and will return
   whatever the function does.
   Typically, the special forms are created by a special form function."
  [value pattern]
  (if (special-form? pattern)
    (apply (second pattern) value (nnext pattern))
    (when (not= value pattern)
      (let [type (some #(and (% pattern) %) (keys matchers))]
        (if (and type (type value))
          ((matchers type) value pattern)
          (report-difference value pattern))))))

;;; Functions that make special forms

(defn- anything [value]
  (when (= value ::nothing)
    (report-difference value "anything")))
(defn- test-pred [value pred]
  (when (not (pred value))
    (report-difference value pred)))

(defn any
  ([] [::test anything])
  ([pred] [::test test-pred pred]))

(defn- differences-as-sets [value pattern]
  (if (and (sequential? value) (sequential? pattern))
    (differences (set value) (set pattern))
    (differences value pattern)))

(defn as-set [pattern] [::test differences-as-sets pattern])

(defn- differences-from-eval [value pattern]
  (let [evaled-value (cond (sequential? value)
                           (current-value (apply (first value) (rest value)))
                           (reporter? value) (current-value value)
                           true value)]
    (differences evaled-value pattern)))

(defn evals-to [pattern] [::test differences-from-eval pattern])

;;; Define check as a macro for the is test.

;;; Used in (is (check <value> <pattern>))
;;; Handled by the method on assert-expr.
(def check) 

(defmethod assert-expr 'check [msg form]
  (let [args (rest form)
        pred (first form)]
    `(let [values# (list ~@args)
           result# (apply differences values#)]
       (if result#
         ;; A non-nil result indicates a failure, and describes it.
         (do-report {:type :fail, :message ~msg,
                  :expected '~form, :actual result#})
         (do-report {:type :pass, :message ~msg,
                  :expected '~form, :actual (cons ~pred values#)}))
       result#)))

(defn set-and-propagate
  "Support function for let-mutated. Given a function of n items
  and a list of n entities in list form, create an empty mutable
  store, give all the enities a content of the empty string,
  call the function with those entities evaluate
  the resulting reporter, then add the correct value and any elements
  to the entities recompute the reporter, returning its value."
  [fun entities]
  (let [s (new-element-store)
        [s ids contents]
        (reduce
         (fn [[store ids contents] entity]
           (let [[new-store new-id] (add-entity store nil "")
                 [new-store new-content] (let [cont (content entity)]
                                           (if (atom? cont)
                                             [new-store cont]
                                             (add-entity new-store nil cont)))]
             [new-store (conj ids new-id) (conj contents new-content)]))
         [s [] []] entities)
        ms (new-mutable-store s)
        md (expression-manager/new-expression-manager-data)
        mutable-entities (map #(cosheet.entity/description->entity % ms) ids)
        result (apply fun mutable-entities)]
    (expression-manager/request result md)
    (expression-manager/compute md)
    (doseq [[mutable-entity entity cont]
            (map vector mutable-entities entities contents)]
      (cosheet.store/update-content!
       ms (:item-id mutable-entity) cont)
      (doseq [element (elements entity)]
        (cosheet.store-utils/add-entity!
         ms (:item-id mutable-entity) element)))
    (expression-manager/compute md)
    (value result)))

;;; A macro to test propagation of changes of an entity through an
;;; expression. Set up the values of the bindings to be entities that
;;; are currently empty, but that will equal the specified entity.
;;; Evaluate exp with a current value of that entity being empty, then
;;; set the entity in the mutable store, recompute, and return the new
;;; current value of the expression.
(defmacro let-mutated [bindings exp]
  (let [binding-pairs (partition 2 bindings)
        vars (vec (map first binding-pairs))
        entities (vec (map second binding-pairs))]
    `(set-and-propagate (fn ~vars ~exp) ~entities)))

;;; A macro to test propagation of changes through a store. Set up var
;;; to a mutable store, initialized from the initial immutable store.
;;; Evaluate exp with the store in that state. Then call the mutator
;;; with the mutable store as an argument, and return the new current
;;; value of the expression.
(defmacro let-mutated-store [[var initial mutator] exp]
  `(let [~var (new-mutable-store ~initial)
         md# (expression-manager/new-expression-manager-data)
         exp-val# ~exp]
     (expression-manager/request exp-val# md#)
     (expression-manager/compute md#)    
     (~mutator ~var)
     (expression-manager/compute md#)
     (value exp-val#)))

(defn item->immutable
  "Given an item, return an immutable version of its current value."
  [item]
  (if (mutable-entity? item)
    (description->entity (:item-id item) (current-store (:store item)))
    item))

