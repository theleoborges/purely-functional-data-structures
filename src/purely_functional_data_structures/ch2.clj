(ns purely-functional-data-structures.ch2)

;;
;;Chapter 2 - Persistence
;;

(defn ++ [xs ys]
  (if (seq xs)
    (cons (first xs) (++ (rest xs) ys))
    ys))


(defn update [xs idx val]
  (if (= 0 idx)
    (cons val (rest xs))
    (cons (first xs) (update (rest xs) (dec idx) val))))

;;
;; Excercise 2.1
;;

(defn suffixes [xs]
  (if (seq xs)
    (cons xs (suffixes (rest xs)))
    (conj [] xs)))

(suffixes [1 2 3 4]) ;; ([1 2 3 4] (2 3 4) (3 4) (4) ())

;;
;; Binary search tree
;;

(defn mk-tree [l v r]
  {:left l :val v :right r})

(defn is-member? [tree value]
  (if-let [member (:val tree)]
    (cond
     (< value member) (recur (:left tree) value)
     (> value member) (recur (:right tree) value)     
     :else true)
    false))


(defn insert [tree value]
  (if-let [member (:val tree)]
    (cond
     (< value member) (mk-tree
                       (insert (:left tree) value) member (:right tree))
     (> value member) (mk-tree (:left tree) member (insert (:right tree) value))
     :else tree)
    (mk-tree nil value nil)))

;;
;; Excercise 2.2
;;

(defn insert [tree value]
  (if-let [member (:val tree)]
    (cond
     (< value member) (mk-tree
                       (insert (:left tree) value) member (:right tree))
     (> value member) (mk-tree (:left tree) member (insert (:right tree) value))
     :else (throw (Exception. "Element already in tree")))
    (mk-tree nil value nil)))


(defn insert* [tree value]
  (try (insert tree value)
       (catch Exception e
         (do (prn "Element already in tree. Halting and returning root")
             tree))))


;;(insert* (insert* my-tree 2) 2)
;; "Element already in tree. Halting and returning root"