(ns purely-functional-data-structures.ch3-test
  (:refer-clojure :exclude [merge])
  (:require [clojure.walk :as w])
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
    (let [time-O-n (get-time (dotimes [_ 10000] (heap-from-list-O-n (range 100))))
          time-O-log-n (get-time (dotimes [_ 10000] (heap-from-list-O-log-n (range 100))))]
      (prn "Creating leftist heap with 100 elemens: ")
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
             expected-min)))))