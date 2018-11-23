(ns browser-interview.core-test
  (:require [clojure.test :refer :all]
            [browser-interview.core :refer :all]))

;; Test cases:
;; Browsing
;; - Creating a store should have -1 index and empty queue
;; - Forward and backward on empty should keep index at -1
;; - After adding several pages (but not max number), forward
;;   and back don't go past the limits
;; - Navigating to a new page partway through the queue will shorten
;;   the queue and append the new page
;; - Navigating to a new page while at the end of the queue will not
;;   change the index.  It will add a new page to the end and remove one
;;   from the head of the queue.

(defn test-site
  [n]
  (format "site-%d.com" n))

;; An empty store should keep index of -1
;; And should not change with fwd/back moves
(deftest empty-store-test
  (let [store (create-store 5)
        back (-> store store-back-move)
        fwd (-> store store-fwd-move)]
    (is (= back fwd))
    (is (= -1 (:index back)))
    (is (nil? (store-get-current store)))))

;; When the store is at max capacity, visiting
;; another page should cause the first page to
;; be dequeued.
(deftest store-overflow-test
  (let [store (reduce store-visit
                      (create-store 5)
                      (map test-site (range 5)))
        _ (is (= 4 (:index store)))
        _ (is (= "site-4.com" (store-get-current store)))
        store (store-visit store "site-5.com")
        _ (is (= 4 (:index store)))
        _ (is (= "site-5.com" (store-get-current store)))
        _ (is (= 5 (count (:queue store))))
        _ (is (= "site-1.com" (first (:queue store))))]))

;; Make sure we can't move past the end of the queue
(deftest fwd-move-tests
  (let [store (reduce store-visit
                      (create-store 5)
                      (map test-site (range 4)))
        _ (is (= 3 (:index store)))
        _ (is (= "site-3.com" (store-get-current store)))

        ;; The store is the same if it is moved forward while at the end
        _ (is (= store (store-fwd-move store)))

        ;; Same is true if we get to max val
        store (store-visit store "site-4.com")
        _ (is (= 4 (:index store)))
        _ (is (= "site-4.com" (store-get-current store)))
        _ (is (= store (store-fwd-move store)))]))

;; Make sure we can't move past the beginning of the queue
(deftest back-move-tests
  (let [store (reduce store-visit
                      (create-store 5)
                      (map test-site (range 5)))
        ;; Moving back 4 pages should put us at zero
        store (nth (iterate store-back-move store) 4)
        _ (is (= 0 (:index store)))
        _ (is (= "site-0.com" (store-get-current store)))
        ;; Moving back again shouldn't be a problem
        _ (is (= store (store-back-move store)))]))

;; Make sure that visiting from the middle erases the
;; URLs ahead of the current URL
(deftest visit-in-middle-tests
  (let [store (reduce store-visit
                      (create-store 5)
                      (map test-site (range 5)))
        store (-> store store-back-move store-back-move)
        _ (is (= 2 (:index store)))
        _ (is (= "site-2.com" (store-get-current store)))
        _ (is (= 5 (count (:queue store))))
        ;; Now that we're at the middle, we'll visit a new site from there
        store (store-visit store "site-5.com")
        _ (is (= 3 (:index store)))
        _ (is (= "site-5.com" (store-get-current store)))
        _ (is (= 4 (count (:queue store))))
        ]))

;; Testing to make sure the autocomplete works as intended
(deftest autocomplete-tests
  (let [store (reduce store-visit
                      (create-store 5)
                      ["google.com"
                       "gmail.com"
                       "bing.com"
                       "amperity.com"
                       "clojure.com"])
        _ (is (= (lookup store "g.com")
                 ["google.com" "gmail.com" "bing.com"]))
        _ (is (= (lookup store "mi")
                 ["gmail.com" "amperity.com"]))
        _ (is (= (lookup store "..")
                 []))
        ;; Since every URL ends in .com, we should get
        ;; them all back
        _ (is (= (lookup store ".com")
                 (vec (:queue store))))]))
