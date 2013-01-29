(ns purely-functional-data-structures.ch2-test
  (:use clojure.test
        purely-functional-data-structures.ch2))

(deftest persistence
  (testing "catenation"
    (is (= (++ [] [4 5 6])
           [4 5 6]))

    (is (= (++ [1 2 3] [4 5 6])
           [1 2 3 4 5 6])))

  (testing "updates"
    (is (= (update [1 2 3 4] 0 7)
           [7 2 3 4]))

    (is (= (update [1 2 3 4] 2 10)
           [1 2 10 4])))


  (testing "suffixes"
    (is (= (suffixes [1 2 3 4])
           '([1 2 3 4] (2 3 4) (3 4) (4) ())))))

(deftest binary-search-tree
  (let [tree (mk-tree
              (mk-tree
               (mk-tree nil 4 nil)
               5
               (mk-tree nil 7 nil))
              10
              (mk-tree
               nil
               15
               (mk-tree nil 20 nil)))]
    (testing "membership"
      (is (is-member? tree 5))
      (is (is-member? tree 10))
      (is (is-member? tree 20))
      (is (not (is-member? tree 3)))
      (is (not (is-member? tree 1))))

    (testing "insert"
      (is (is-member? (insert tree 13) 13))
      (is (is-member? (insert tree 9) 9)))

    (testing "using exceptions to abort when inserting duplicates"
      (is (thrown-with-msg? Exception #"Element already in tree"
            (insert (insert tree 2) 2))))))