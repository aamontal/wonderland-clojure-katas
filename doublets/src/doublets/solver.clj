(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core :as core]
            [clojure.set :as cset]))

(defn to-string [s]  (clojure.string/join s))

(def alphabet (map char (range 97 123)))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(def vec-dict
  (set (map (fn [x] (vec x)) words) ))

(defn word-to-vec [word]
  (vec word))

(defn neighbors [word vec-dict]
  (let [vec-word (word-to-vec word)]
    (for
      [ i (range 0 (count vec-word))
        c alphabet
        :let  [new-word (assoc vec-word i c)]
        :when (and (contains? vec-dict new-word)
                   (not (= new-word vec-word))) ]
      (to-string new-word))))

(defn get-frontier [words vec-dict]
  (set (flatten (map (fn [w] (neighbors w vec-dict)) words ))))

(defn unvisited [visited frontier-neighbors]
   (filter (fn [n] (not (contains? visited n) )) frontier-neighbors))

(defn bfs [frontier dict visited target route]
  (cond
    (empty? frontier) []
    :else
      (let [frontier-neighbors (get-frontier frontier dict)
            new-neighbors (unvisited visited frontier-neighbors) ]
        (if (contains? (set new-neighbors) target)
          route
          (bfs new-neighbors
               dict
               (cset/union visited (set frontier))
               target
               (cons new-neighbors route))))))

(defn path-back [target route]
  (if (empty? route)
    []
    (let [nbrs (get-frontier target vec-dict)
          word-link (cset/intersection (set nbrs) (set (first route))) ]
      (concat (path-back word-link (rest route) )
          (list (first link)) ))))

(defn doublets [word1 word2]
  (let [route (bfs [word1] vec-dict #{} word2 '()) ]
    (if (empty? route)
      []
      (concat (list word1)
              (path-back (list word2) route)
              (list word2)))))
