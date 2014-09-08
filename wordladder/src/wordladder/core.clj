(ns wordladder.core
(:require [clojure.java.io :refer :all])
(:gen-class))

(def alphabet (set (map char (range 97 123))))

(defn read-word-set []
	(set (with-open [rdr (reader "/Users/gt83984/projects/dojos/CodingDojoWeek43/word.list")]
		(doall (line-seq rdr)
			))))

(def dictionary (read-word-set))

(defn find-word [word word-set]
	(contains? word-set word)
	)

(defn all-other-letters [letter]
	(disj alphabet letter)
	)
(defn vector-contains [previous-words]
	(fn [element]
		(some #(= element %) previous-words))
	)

(def call-map (atom {}))

(defn increment-calls [call-name] 
	(swap! call-map (fn [current-call-map] (assoc current-call-map call-name (inc (get current-call-map call-name 0))))))

(defn output-count [[entry value]]
	(str entry " => " value "\n"))

(defn output-counts []
	(reduce str (map output-count @call-map)))

(defn generate-next-words [word previous-words]
	(increment-calls "generate-next-words")
	(filter dictionary (remove (vector-contains previous-words) (let [letters (apply vector word)]
		(for [i (range 0 (count letters))
			  replacement (all-other-letters (nth letters i))]
			(apply str (assoc letters i replacement)))))))

(defn filter-words-in-list [listGen listOrg]
   (filter listOrg listGen)
)

(defn reverse-link-list [end-word word-vector]
	(loop [current-word end-word
		path [end-word]]
		(let [parent ((first (filter #(= current-word (% :word)) word-vector)) :parent)]
			(if (= parent current-word)
				path
				(recur parent (cons parent path))))))

(defn inner-solve [start-word end-word word-vector]
	(increment-calls "inner-solve")
	(loop [index 1
		 path word-vector]
		 (let [current-word ((nth path index) :word)]
		 	(if (some #(= end-word (% :word)) path)
		 		(reverse-link-list end-word path)
		 		(recur (inc index) (concat path (map #(hash-map :word %, :parent current-word) (generate-next-words current-word (map :word path)))))
			)	
		)
	)
)

(defn solve [start-word end-word]
	(let [result (time
	 	(if (= start-word end-word)
		  start-word
		  (inner-solve start-word end-word
		   (map #(hash-map :word %, :parent start-word)
		   (conj (generate-next-words start-word []) start-word)))))]
		(println "output-counts:" (output-counts))
		result))
