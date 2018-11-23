(ns browser-interview.core
  (:require [clojure.string :refer [escape]])
  (:import [clojure.lang PersistentQueue]))

;; Note: You may implement this in python, Javascript or any other language.

;; You may also decide to create class or object to wrap this _ your call!

;; Browser history: Build the object to support browser history.
;; Browser should support URL bar, forward and back buttons
;; No need for network calls - this is a data structure only.

;; creates the history object _ use a class, record, or other structure if you want!

;; EXAMPLE: 
;; IF: 
;;  - visit A
;;  - visit B
;;  - visit C
;; THEN:
;;   - back, back yields A.
;; THEN:
;;   - forward, forward yields C.
;; THEN:
;;   - back, visit D eliminates C from the history.

;; stores at most `max-count` URLs in the history
;; eliminates the element furthest back in the history.


;; 
(defrecord UrlStore [max-count index queue])

(defmethod print-method UrlStore
  [store writer]
  (.write writer (format "UrlStore(max: %d, idx: %d, pages: %s)"
                      (:max-count store)
                      (:index store)
                      (vec (:queue store)))))

(defn create-store [max-count]
  (map->UrlStore {:max-count max-count
                  :index -1 ; Index
                  :queue PersistentQueue/EMPTY}))

;; to support "click a link" on the browser
(defn store-visit
  [{:keys [max-count index queue] :as store-object}
   url]
  (let [end-idx (dec (min (count queue) max-count))
        [new-idx
         new-queue] (cond
                      ;; If we're partway through, take up to the current
                      ;; element and put them into a new queue
                      (< index end-idx)
                      [(inc index)
                       (into PersistentQueue/EMPTY
                             (take (inc index) queue))]
                      ;; If we're at the end, pop the first element
                      (= index (dec max-count))
                      [index
                       (pop queue)]
                      ;; Otherwise, just  the queue itself
                      :else
                      [(inc index)
                       queue])]
    (assoc store-object
           :index new-idx
           :queue (conj new-queue url))))

;; to support "go back" on the browser
(defn store-back-move
  [store-object]
  (update store-object
          :index
          #(if (> % 0) (dec %) %)))

;; to support "go forward" on the browser
(defn store-fwd-move
  [{:keys [index queue]
    :as store-object}]
  (assoc store-object
         :index
         (min (inc index) (dec (count queue)))))

(defn store-get-current
  "Gets the current page."
  [{:keys [queue index] :as store-object}]
  (nth queue index nil))


;; Bonus: look up matching URLs by substring
;; This substring method will use *partial* substring method
(declare make-regex)
(defn lookup [store-object substring]
  (let [regex (make-regex substring)]
    (filter #(re-find regex %)
            (:queue store-object))))

;; Helper functions

;; Code snippet from: https://gist.github.com/cljforge/855b95abbb7a289144d7
(def ^:private char-esc-map
  (let [esc-chars "()&^%$#!?*."]
    (zipmap (map str esc-chars)
            (map #(str "\\" %) esc-chars))))

(defn ^:private make-regex
  "Turns a string into a regex where .* is between
  each character of the original string.
  (i.e. 'abc' becomes 'a.*b.*c')
  This allows us to match strings where all the
  characters appear in the same order, even if they
  are not contiguous.
  (e.g. 'Apty' will match 'Amperity')
  This function automatically handles excaping of
  special regex characters in the original string."
  [s]
  (->> s
       (map str)
       (replace char-esc-map)
       (interpose ".*")
       (apply str)
       (re-pattern)))
