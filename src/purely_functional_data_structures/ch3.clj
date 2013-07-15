(ns purely-functional-data-structures.ch3
  (:refer-clojure :exclude [merge])
  (:use [clojure.core.match :only (match)]
        [clojure.pprint     :only (pprint)]))


;;
;; Chapter 3 - Familiar Data Structures in a Functional Setting
;;


;;
;; Leftist heaps
;;

;;
;; Using protocols
;;


(defprotocol Heap
  (is-empty?   [this])
  (insert      [this v])
  (merge       [this other])
  (rank        [this])
  (find-min    [this])
  (delete-min  [this]))

(defrecord LeftistHeap [rank value left right])

(defn ensure-leftist
 [this other v]
 (let [rank-this (rank this)
       rank-other (rank other)]
   (if (>= rank-this rank-other)
     (->LeftistHeap (inc rank-other) v this other)
     (->LeftistHeap (inc rank-this) v other this))))

(extend-protocol Heap
  nil
  (rank [_] 0)
  (merge [_ other] other)
  (is-empty? [_] true)

  LeftistHeap
  (is-empty? [this]
    (nil? this))

  (rank [this]
    (:rank this))
  
  (merge [{val-this :value left-this :left right-this :right :as this}
          {val-other :value left-other :left right-other :right :as other}]
    (cond
     (is-empty? other) this
     (<= val-this val-other) (ensure-leftist left-this
                                             (merge right-this other)
                                             val-this)
     :else (ensure-leftist left-other
                           (merge this right-other)
                           val-other)))
  
  (insert [this v]
    (merge (->LeftistHeap 1 v nil nil)
           this))

  (find-min [{v :value}] v)
  
  (delete-min [{left :left right :right}]
    (merge right left)))


;;
;; Using pure functions and maps
;;


(defn mk-heap [rank value left right]
  {:rank rank :value value :left left :right right})

(defn heap-rank [heap]
  (if (nil? heap)
    0
    (:rank heap)))

(defn ensure-leftist-heap [value heap-a heap-b]
  (let [rank-a (heap-rank heap-a)
        rank-b (heap-rank heap-b)]
    (if (>= rank-a rank-b)
      (mk-heap (inc rank-b) value heap-a heap-b)
      (mk-heap (inc rank-a) value heap-b heap-a))))

(defn merge-heaps [{val-a :value left-a :left right-a :right :as heap-a}
                   {val-b :value left-b :left right-b :right :as heap-b}]
  (cond
   (nil? heap-a) heap-b
   (nil? heap-b) heap-a
   (<= val-a val-b) (ensure-leftist-heap val-a
                                         left-a
                                         (merge-heaps right-a heap-b))
   :else (ensure-leftist-heap val-b
                              left-b
                              (merge-heaps heap-a right-b))))

(defn heap-insert [value heap]
  (merge-heaps (mk-heap 1 value nil nil)
               heap))

(defn heap-find-min [{v :value}] v)
  
(defn heap-delete-min [{left :left right :right}]
  (merge-heaps right left))


;;
;; Exercises - p19
;;

;;
;; 3.2 
;;

(defn direct-heap-insert
  [value {val-b :value left-b :left right-b :right :as heap-b}]
  (cond (nil? heap-b) (mk-heap 1 value nil nil)
        (< value val-b) (ensure-leftist-heap value heap-b nil)
        :else (ensure-leftist-heap val-b left-b
                                   (direct-heap-insert value right-b))))

;;
;; 3.3
;;

(defn heap-from-list-O-n
  [coll]
  (reduce (fn [acc i]
            (merge-heaps (if (map? acc) acc (mk-heap 1 acc nil nil))
                         (mk-heap 1 i nil nil))) coll))


(defn mk-singleton-heap [n]
  (mk-heap 1 n nil nil))

(defn heap-from-list-O-log-n
  [coll]
  (let [singleton-heaps (map mk-singleton-heap coll)]
    (loop [heaps singleton-heaps]
      (if (= (count heaps) 1)
        (first heaps)
        (recur (map (fn [pair] (apply merge-heaps pair))
                    (partition 2 2 [nil] heaps)))))))



;;
;; Binomial Heaps
;;

(defn mk-binomial-heap [rank value children]
  {:rank rank :value value :children children})


(defn link-binomial-heaps [{rank :rank value-a :value children-a :children :as heap-a}
                           {value-b :value children-b :children :as heap-b}]
  (if (<= value-a value-b)
    (mk-binomial-heap (inc rank) value-a (cons heap-b children-a))
    (mk-binomial-heap (inc rank) value-b (cons heap-a children-b))))

(defn insert-into-binomial-heap* [heap [head & tail :as heaps]]
  (if (or (empty? heaps)
          (< (heap-rank heap) (:rank head)))  (cons heap heaps)
          (insert-into-binomial-heap* (link-binomial-heaps heap head)
                                      tail)))

(defn insert-into-binomial-heap [value heaps]
  (insert-into-binomial-heap* (mk-binomial-heap 0 value [])
                              heaps))

(defn merge-binomial-heaps [[{rank-heap-a :rank :as heap-a} & tail-a :as heaps-a]
                            [{rank-heap-b :rank :as heap-b} & tail-b :as heaps-b]]
  (cond
   (empty? heaps-a) heaps-b
   (empty? heaps-b) heaps-a   
   (< rank-heap-a rank-heap-b) (cons heap-a (merge-binomial-heaps tail-a heaps-b))
   (< rank-heap-b rank-heap-a) (cons heap-b (merge-binomial-heaps heaps-a tail-b))
   :else (insert-into-binomial-heap* (link-binomial-heaps heap-a heap-b)
                                     (merge-binomial-heaps tail-a tail-b))))

(defn remove-min-binomial-heap [[{value-heap-a :value :as heap-a} & tail-a :as heaps-a]]
  (cond (empty? heaps-a) (throw (Exception. "Empty binomial heap"))
        (= 1 (count heaps-a)) [heap-a []]
        :else (let [[{value-heap-b :value :as heap-b} heaps-b] (remove-min-binomial-heap tail-a)]
                (if (< value-heap-a value-heap-b)
                  [heap-a heaps-b]
                  [heap-b (cons heap-a heaps-b)]))))

(defn find-min-binomial-heap [heaps]
  (first (remove-min-binomial-heap heaps)))

(defn delete-min-binomial-heap [heaps]
  (let [[{children :children} rest] (remove-min-binomial-heap heaps)]
    (merge-binomial-heaps (reverse children)
                          rest)))

(defn binomial-heap-from-list [coll]
  (reduce (fn [acc n]
            (insert-into-binomial-heap n acc))
          []
          coll))

;;
;; Exercises - p23
;;

;;
;; 3.5 - Define findMin directly rather than via a call to removeMinTree.
;;

;; First a recursive solution

(defn rec-find-min-binomial-heap  [[heap-a & rest]]
  (if (seq rest)
    (let [heap-b (rec-find-min-binomial-heap rest)] 
      (if (<= (:value heap-a) (:value heap-b))
        heap-a
        heap-b))
    heap-a))

;; Now using reduce

(defn reduce-find-min-binomial-heap [heaps]
  (reduce (fn [acc heap]
            (if (<= (:value acc) (:value heap))
              acc
              heap))
          heaps))

;;
;; 3.6 - Most of the rank annotation in this representation of
;; binomial heaps are redundant because we know that the children of
;; a node of rank r have ranks r-1,...,0 . Thus, we can remove the rank
;; annotations from each node and instead pair each tree at the top-level with its rank, i.e.,
;;
;; datatype Tree = Node of Elem  x Tree list 
;; type Heap = (int x Tree) list
;;
;; Reimplement binomial heaps with this new representation.
;;


(defn mk-node [value children]
  {:value value :children children})

(defn link-bin-heaps [[rank {value-a :value children-a :children :as heap-a}]
                      [_    {value-b :value children-b :children :as heap-b}]]
  (if (<= value-a value-b)
    [(inc rank) (mk-node value-a (cons heap-b children-a))]
    [(inc rank) (mk-node value-b (cons heap-a children-b))]))

(defn insert-into-bin-heap* [[rank-a _ :as heap-a]
                             [[rank-b _ :as head-heap-b] & tail :as heaps-b]]
  (if (or (empty? heaps-b)
          (< rank-a rank-b))  (cons heap-a heaps-b)
          (insert-into-bin-heap* (link-bin-heaps heap-a head-heap-b)
                                      tail)))

(defn insert-into-bin-heap [value heaps]
  (insert-into-bin-heap* [0 (mk-node value [])]
                         heaps))


(defn bin-heap-from-list [coll]
  (reduce (fn [acc n]
            (insert-into-bin-heap n acc))
          []
          coll))

(defn merge-bin-heaps [[[rank-a _ :as heap-a] & tail-a :as heaps-a]
                       [[rank-b _ :as heap-b] & tail-b :as heaps-b]]
  (cond
   (empty? heaps-a) heaps-b
   (empty? heaps-b) heaps-a   
   (< rank-a rank-b) (cons heap-a (merge-bin-heaps tail-a heaps-b))
   (< rank-b rank-a) (cons heap-b (merge-bin-heaps heaps-a tail-b))
   :else (insert-into-bin-heap* (link-bin-heaps heap-a heap-b)
                                     (merge-bin-heaps tail-a tail-b))))

(defn remove-min-bin-heap [[[_ {value-heap-a :value} :as heap-a] & tail-a :as heaps-a]]
  (cond (empty? heaps-a) (throw (Exception. "Empty binomial heap"))
        (= 1 (count heaps-a)) [heap-a []]
        :else (let [[[_ {value-heap-b :value} :as heap-b] heaps-b] (remove-min-bin-heap tail-a)]
                (if (< value-heap-a value-heap-b)
                  [heap-a heaps-b]
                  [heap-b (cons heap-a heaps-b)]))))

(defn find-min-bin-heap [heaps]
  (first (remove-min-bin-heap heaps)))

(defn decorate-heaps-with-rank [rank heaps]
  (second (reduce (fn [[r result] heap]
                    (let [new-rank (dec r)]
                      [new-rank (conj result [new-rank heap])]))
                  [rank []]
                  heaps)))

(defn delete-min-bin-heap [heaps]
  (let [[[min-rank {children :children}] rest] (remove-min-bin-heap heaps)]
    (merge-bin-heaps (reverse (decorate-heaps-with-rank min-rank children))
                     rest)))


;;
;; Red-Black trees
;;


(defn rb-mk-tree [color left value right]
  {:color color :left left :value value :right right})


(def rb-tree (rb-mk-tree :black
                         (rb-mk-tree :red ;; left
                                     (rb-mk-tree :black nil "a" nil) ;;left
                                     8
                                     (rb-mk-tree :red ;;right
                                                 (rb-mk-tree :black nil "b" nil) ;;left
                                                 11
                                                 (rb-mk-tree :black nil "c" nil))) ;;right
                         13
                         (rb-mk-tree :black nil "d" nil) ;;right
                         ))



(comment

  ;; My first attempt - far from ideal without pattern-matching 
  
  (defn rb-balance [tree]
    (let [{z :value d :right} tree
          {x-color :color x :value a :left} (-> tree :left)
          {y-color :color y :value b :left c :right} (-> tree :left :right)
          d (:right tree)]
      (if (and (= x-color :red) (= y-color :red))
        (rb-mk-tree :red
                    (rb-mk-tree :black a x b)
                    y
                    (rb-mk-tree :black c z d))
        tree))))




(defn rb-balance [tree]
  (match [tree]
         [(:or {:left {:color :red
                       :left {:color :red :left a :value x :right b}
                       :value y :right c}
                :value z :right d}

               {:left {:color :red                    
                       :left  a :value x
                       :right {:color :red :value y :left b :right c}}
                :value z :right d}

               {:left a :value x
                :right {:color :red
                        :left {:color :red
                               :left b :value y :right c}
                        :value z :right d}}

               {:left a :value x
                :right {:color :red
                        :left b :value y
                        :right {:color :red
                                :left c :value z :right d}}})]
         (rb-mk-tree :red
                     (rb-mk-tree :black a x b)
                     y
                     (rb-mk-tree :black c z d))

         :else tree))

(defn balance [tree]
  (match [tree]
         [(:or [:black [:red [:red a x b] y c] z d]
               [:black [:red a x [:red b y c]] z d]
               [:black a x [:red [:red b y c] z d]]
               [:black a x [:red b y [:red c z d]]])] [:red [:black a x b]
                                                            y
                                                            [:black c z d]]
               :else tree))

(defn insert [tree x]
  (let [ins (fn ins [tree]
              (match tree
                     nil [:red nil x nil]
                     [color a y b] (cond
                                    (< x y) (balance [color (ins a) y b])
                                    (> x y) (balance [color a y (ins b)])
                                    :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

(defn is-member? [tree x]
  (match tree
         nil false
         [_ a y b] (cond
                    (< x y) (recur a x)
                    (> x y) (recur b x)     
                    :else true)))

(comment

  ;; When core.match supports protocols, this will be
  ;; awesome.
  
  (defrecord Black [left value right])
  (defrecord Red [left value right])


  (def protocol-tree
    (Black. (Red.
             (Black. nil "a" nil)
             8
             (Red.
              (Black. nil "b" nil)
              11
              (Black. nil "c" nil)))
            13
            (Black. nil "d" nil)))

  (defn balance [node]
    (match [node]
           [(:or (Black. (Red. (Red. a x b) y c) z d)
                 (Black. (Red. a x (Red. b y c)) z d)
                 (Black. a x (Red. (Red. b y c) z d))
                 (Black. a x (Red. b y (Red. c z d))))] (Red. (Black. a x b)
                                                              y
                                                              (Black. c z d))
                 :else node)))