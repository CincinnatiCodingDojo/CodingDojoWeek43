(ns wordladder.core-test
  (:require [midje.sweet :refer :all]
            [wordladder.core :refer :all]))

(def word-set (read-word-set))

(fact "can read from a static word list file" 
	(count word-set) => 264097)

(fact "can find determine if a word is valid"
	(find-word "test" #{ "test" "other" "words" }) => true
	(find-word "best" #{ "test" "other" "words" }) => false)

(fact "gimme all the letters in the alphabet besides the given letter"
	(all-other-letters \a) =not=> (contains \a)
	(count (all-other-letters \a)) => 25
	)

(fact "test valid next word"
	(let [words (generate-next-words "bart" [])]
		words => (contains "cart")
		words =not=> (contains "bert")
		words => (contains "barn")
		words =not=> (contains "bart")
		words =not=> (contains "eark")))

(fact "can return only valid words from the word list"
	(let [next-words (generate-next-words "bart" [])]
		(filter-words-in-list next-words #{"bart" "bait"}) => (contains "bait")
		(filter-words-in-list next-words #{"bart" "beat"}) =not=> (contains "beat")
		(filter-words-in-list next-words #{"bart" "barf"}) =not=> (contains "bact")
	))

(fact "a valid new word can not be one of the words we'''''''ve already generated"
	(generate-next-words "bart" ["bart" "bait"]) =not=> (contains "bait")
	)	

(fact "a link list containing parents and words can be reversed in the correct direction"
	(reverse-link-list "text" (vector {:word "text" :parent "test"} {:word "test" :parent "test"})) => ["test" "text"] 
	)

(fact "when start-word and end-word are the same then returs start-word"
	(solve "a" "a") => "a"
	)

(fact "returns list with correct words"
	(solve "at" "it") => ["at" "it"]
    (solve "cat" "sin") => ["cat" "sat" "sit" "sin"]
	)

(fact "increment calls "
	(reset! call-map {})
	(increment-calls "anything") => {"anything" 1}
	(increment-calls "anything") => {"anything" 2}
	(reset! call-map {})
	)

(fact "outputs the counts"
(reset! call-map {})
	(increment-calls "anything") => {"anything" 1}

	(output-counts ) => "anything => 1\n" 

(reset! call-map {})
	)