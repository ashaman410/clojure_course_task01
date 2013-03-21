(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn select-branch? [x]
  (keyword? (first x)))
          
(defn select-children [x]
  (rest (rest x)))

(defn attrs [x]
  (second x))

(defn link-tag? [x]
  (and (= (first x) :a)
       (= (:class (attrs x)) "l")))

(defn link [x]
  (:href (attrs x)))

(defn nodes-sequence [x]
  (let [walk 
        (fn walk [node] 
          (lazy-seq (cons node
                          (when (select-branch? node) 
                            (mapcat walk (select-children node))))))]
    (walk x)))

(defn filter-links [x]
  (map link (filter link-tag? (nodes-sequence x))))
       
(defn get-links []
  (let [data (parse "clojure_google.html")]
    (filter-links data)))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))  
