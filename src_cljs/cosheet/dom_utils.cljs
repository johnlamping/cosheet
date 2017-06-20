(ns cosheet.dom-utils)

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
