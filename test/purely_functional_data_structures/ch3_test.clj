(ns purely-functional-data-structures.ch3-test
  (:refer-clojure :exclude [merge])
  (:require [clojure.walk :as w]
            [clojure.zip :as z])
  (:use clojure.test
        purely-functional-data-structures.ch3))

(defn heap-values [heap]
  (map :value (filter (complement nil?)
                      (tree-seq map? (fn [node] (vector (:left node) (:right node)))
                                heap))))



(defmacro get-time
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(deftest leftist-heaps
  (testing "Implemented with protocols"
    (let [heap (-> (->LeftistHeap 1 3 nil nil)
                   (insert 2)
                   (insert 7)
                   (insert 4)
                   (insert 10)
                   (insert 1)
                   (insert 20))]

      (is (= (heap-values heap)
             [1 2 4 7 10 3 20]))

      (is (= (find-min heap)
             1))

      (is (= (:value (delete-min heap))
             2))))


  (testing "Implemented with pure functions"
    (let [heap (->> (mk-heap 1 3 nil nil)
                    (heap-insert 2)
                    (heap-insert 7)
                    (heap-insert 4)
                    (heap-insert 10)
                    (heap-insert 1)
                    (heap-insert 20))]

      (is (= (heap-values heap)
             [1 2 4 7 10 3 20]))

      (is (= (heap-find-min heap)
             1))

      (is (= (:value (heap-delete-min heap))
             2))))

  (testing "Implementing insert directly"
    (let [heap (->> (mk-heap 1 3 nil nil)
                    (direct-heap-insert 2)
                    (direct-heap-insert 7)
                    (direct-heap-insert 4)
                    (direct-heap-insert 10)
                    (direct-heap-insert 1)
                    (direct-heap-insert 20))]
      (is (= (heap-values heap)
             [1 2 4 7 10 3 20]))))

  (testing "Performance"
    (let [time-O-n (get-time (dotimes [_ 10000] (heap-from-list-O-n (range 500))))
          time-O-log-n (get-time (dotimes [_ 10000] (heap-from-list-O-log-n (range 500))))]
      (prn "Creating a leftist heap with 500 elemens 10k times: ")
      (prn (format "O(n) time: %s" time-O-n))
      (prn (format "O(log n) time: %s" time-O-log-n))      
      (is (< time-O-log-n time-O-n)))))


(defn binomial-heap-values [heaps]
  (letfn [(heap-values [heap]
            (map :value (tree-seq map?
                                  (fn [h] (:children h))
                                  heap)))]
    (mapcat heap-values heaps)))

(deftest binomial-heaps
  (testing "linking heaps"
    (let [heap-a (mk-binomial-heap 0 3 [])
          heap-b (mk-binomial-heap 0 2 [])]

      (is (= (link-binomial-heaps heap-a heap-b)
             {:rank 1,
              :value 2,
              :children '({:rank 0,
                           :value 3,
                           :children []})}))

      (is (= (binomial-heap-values
              [(->> (link-binomial-heaps heap-a heap-b)
                    (link-binomial-heaps (mk-binomial-heap 0 1 [])))])
             [1 2 3]))))

  (testing "inserting"
    (let [heaps (->> (insert-into-binomial-heap 1 [])
                    (insert-into-binomial-heap 2)
                    (insert-into-binomial-heap 7)
                    (insert-into-binomial-heap 5)
                    (insert-into-binomial-heap 6)
                    (insert-into-binomial-heap 10)
                    (insert-into-binomial-heap 8))]
      (is (= (count heaps)
             3))
      (is (= (map :rank heaps)
             [0 1 2]))
      (is (= (binomial-heap-values
              heaps)
             [8 6 10 1 5 7 2]))

      (is (= heaps)
          (binomial-heap-from-list [1 2 7 5 6 10])))


    (let [heaps (binomial-heap-from-list [9 5 17 21 99 12 23 12 77 33 24 23 53])]
      (is (= (count heaps)
             3))
      (is (= (map :rank heaps)
             [0 2 3]))
      (is (= (binomial-heap-values
              heaps)
             [53 23 33 77 24 5 12 12 99 23 17 21 9]))))

  (testing "merging heaps"
    (let [heap-a (binomial-heap-from-list [1 2 7 5 6 10 8])
          heap-b (binomial-heap-from-list [9 5 17 21 99 12 23 12 77 33 24 23 53])

          merged-heap (merge-binomial-heaps heap-a heap-b)]
      (is (= (count merged-heap)
             2))

      (is (= (map :rank merged-heap)
             [2 4]))))
  
  (testing "removing min heap"
    (let [heaps (binomial-heap-from-list [1 2 7 5 6 10 8])
          [min rest] (remove-min-binomial-heap heaps)]
      (is (= min
             {:rank 2, :value 1,
              :children [{:rank 1, :value 5,
                          :children [{:rank 0, :value 7,
                                      :children []}]}
                         {:rank 0, :value 2,
                          :children []}]}))

      (is (= (count rest)
             2))))

  (testing "delete min heap"
    (let [heaps (binomial-heap-from-list [1 2 7 5 6 10 8])
          new-heap (delete-min-binomial-heap heaps)]
      (is (= (binomial-heap-values new-heap)
             [2 8 5 6 10 7]))

      (is (= (count new-heap)
             2))))
  
  (testing "finding min heap"
    (let [heaps (binomial-heap-from-list [1 2 7 5 6 10 8])
          expected-min {:rank 2, :value 1,
                        :children [{:rank 1, :value 5,
                                    :children [{:rank 0, :value 7,
                                                :children []}]}
                                   {:rank 0, :value 2,
                                    :children []}]}]
      (is (= (find-min-binomial-heap heaps)
             expected-min))

      (is (= (rec-find-min-binomial-heap heaps)
             expected-min))

      (is (= (reduce-find-min-binomial-heap heaps)
             expected-min))))


  (testing "exercise. 3.6 - inserting"
    (let [heaps (->> (insert-into-bin-heap 1 [])
                     (insert-into-bin-heap 2)
                     (insert-into-bin-heap 7)
                     (insert-into-bin-heap 5)
                     (insert-into-bin-heap 6)
                     (insert-into-bin-heap 10)
                     (insert-into-bin-heap 8))]
      (is (= (count heaps)
             3))
      (is (= (map first heaps)
             [0 1 2]))
      (is (= (binomial-heap-values
              (map second heaps))
             [8 6 10 1 5 7 2]))

      (is (= heaps)
          (bin-heap-from-list [1 2 7 5 6 10])))


    (let [heaps (bin-heap-from-list [9 5 17 21 99 12 23 12 77 33 24 23 53])]
      (is (= (count heaps)
             3))
      (is (= (map first heaps)
             [0 2 3]))
      (is (= (binomial-heap-values
              (map second heaps))
             [53 23 33 77 24 5 12 12 99 23 17 21 9]))))

  (testing "exercise. 3.6 - merging heaps"
    (let [heap-a (bin-heap-from-list [1 2 7 5 6 10 8])
          heap-b (bin-heap-from-list [9 5 17 21 99 12 23 12 77 33 24 23 53])

          merged-heap (merge-bin-heaps heap-a heap-b)]
      (is (= (count merged-heap)
             2))

      (is (= (map first merged-heap)
             [2 4]))))


  (testing "exercise. 3.6 - removing min heap"
    (let [heaps (bin-heap-from-list [1 2 7 5 6 10 8])
          [min rest] (remove-min-bin-heap heaps)]
      (is (= min
             [2 {:value 1,
                 :children [{:value 5,
                             :children [{:value 7, :children []}]}
                            {:value 2,
                             :children []}]}]))

      (is (= (count rest)
             2))))

  (testing "exercise. 3.6 - delete min heap"
    (let [heaps (bin-heap-from-list [1 2 7 5 6 10 8])
          new-heap (delete-min-bin-heap heaps)]
      (is (= (binomial-heap-values (map second new-heap))
             [2 8 5 6 10 7]))

      (is (= (count new-heap)
             2))))
  
  (testing "exercise. 3.6 - finding min heap"
    (let [heaps (bin-heap-from-list [1 2 7 5 6 10 8])
          expected-min [2 {:value 1,
                           :children [{:value 5,
                                       :children [{:value 7, :children []}]}
                                      {:value 2,
                                       :children []}]}]]
      (is (= (find-min-bin-heap heaps)
             expected-min)))))

(defn assert-balanced [balanced]
          (is (= (:value balanced) "y")) ; root
          (is (= (:color balanced) :red)) ; root
          
          (is (= (-> balanced :left :value) "x")) ; left child
          (is (= (-> balanced :left :color) :black)) ; left child        
          (is (= (-> balanced :left :left :value) "a")) ; left's left child
          (is (= (-> balanced :left :right :value) "b")) ; left's right child
          
          (is (= (-> balanced :right :value) "z")) ; right child
          (is (= (-> balanced :right :color) :black)) ; right child        
          (is (= (-> balanced :right :left :value) "c")) ; right's left child
          (is (= (-> balanced :right :right :value) "d")) ; right's left child        
)

(deftest red-black-trees
  (testing "Map-based red-black trees"
    (testing "balancing, case 1"
      (let [tree (rb-mk-tree :black
                             (rb-mk-tree :red ;; left
                                         (rb-mk-tree :black nil "a" nil) ;;left
                                         "x"
                                         (rb-mk-tree :red ;;right
                                                     (rb-mk-tree :black nil "b" nil) ;;left
                                                     "y"
                                                     (rb-mk-tree :black nil "c" nil))) ;;right
                             "z"
                             (rb-mk-tree :black nil "d" nil) ;;right
                             )]
        (assert-balanced (rb-balance tree))))

    (testing "balancing, case 2"
      (let [tree (rb-mk-tree :black
                             (rb-mk-tree :red
                                         (rb-mk-tree :red
                                                     (rb-mk-tree :black nil "a" nil)
                                                     "x"
                                                     (rb-mk-tree :black nil "b" nil))
                                         "y"
                                         (rb-mk-tree :black nil "c" nil))
                             "z"
                             (rb-mk-tree :black nil "d" nil))]
        (assert-balanced (rb-balance tree))))

    (testing "balancing, case 3"
      (let [tree (rb-mk-tree :black
                             (rb-mk-tree :black nil "a" nil)
                             "x"
                             (rb-mk-tree :red
                                         (rb-mk-tree :red
                                                     (rb-mk-tree :black nil "b" nil)
                                                     "y"
                                                     (rb-mk-tree :black nil "c" nil))
                                         "z"
                                         (rb-mk-tree :black nil "d" nil)))]
        (assert-balanced (rb-balance tree))))

    (testing "balancing, case 4"
      (let [tree (rb-mk-tree :black
                             (rb-mk-tree :black nil "a" nil)
                             "x"
                             (rb-mk-tree :red
                                         (rb-mk-tree :black nil "b" nil)
                                         "y"
                                         (rb-mk-tree :red
                                                     (rb-mk-tree :black nil "c" nil)
                                                     "z"
                                                     (rb-mk-tree :black nil "d" nil))))]
        (assert-balanced (rb-balance tree)))))


  (testing "Vector-based red-black trees"
    (letfn [(assert-balanced [balanced]
              (let [balanced-zp (z/zipper vector? seq (fn [_ c] c) balanced)
                    left-child  (comp z/right z/down)
                    right-child (comp z/right z/right z/right z/down)
                    value       (comp z/right z/right z/down)
                    color       z/down]
                ;; root
                (is (= (first (-> balanced-zp color)) :red))
                (is (= (first (-> balanced-zp value)) "y"))


                (is (= (first (-> balanced-zp left-child color)) :black))
                (is (= (first (-> balanced-zp left-child  value)) "x"))


                (is (= (first (-> balanced-zp left-child left-child value)) "a"))
                (is (= (first (-> balanced-zp left-child right-child value)) "b"))


                (is (= (first (-> balanced-zp right-child color)) :black))
                (is (= (first (-> balanced-zp right-child value)) "z"))

                (is (= (first (-> balanced-zp right-child left-child value)) "c"))
                (is (= (first (-> balanced-zp right-child right-child value)) "d"))))]

      (testing "balancing, case 1"
        (let [tree [:black
                    [:red
                     [:black nil "a" nil]
                     "x"
                     [:red
                      [:black nil "b" nil]
                      "y"
                      [:black nil "c" nil]]]
                    "z"
                    [:black nil "d" nil]]]

          (assert-balanced (balance tree))))

      (testing "balancing, case 2"
        (let [tree [:black
                    [:red
                     [:red
                      [:black nil "a" nil]
                      "x"
                      [:black nil "b" nil]]
                     "y"
                     [:black nil "c" nil]]
                    "z"
                    [:black nil "d" nil]]]

          (assert-balanced (balance tree))))

      (testing "balancing, case 3"
        (let [tree [:black
                    [:black nil "a" nil]
                    "x"
                    [:red
                     [:red
                      [:black nil "b" nil]
                      "y"
                      [:black nil "c" nil]]
                     "z"
                     [:black nil "d" nil]]]]

          (assert-balanced (balance tree))))

      (testing "balancing, case 4"
        (let [tree [:black
                    [:black nil "a" nil]
                    "x"
                    [:red
                     [:black nil "b" nil]
                     "y"
                     [:red
                      [:black nil "c" nil]
                      "z"
                      [:black nil "d" nil]]]]]

          (assert-balanced (balance tree)))))))
