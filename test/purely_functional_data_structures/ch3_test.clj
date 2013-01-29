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
