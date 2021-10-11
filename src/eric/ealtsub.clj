(ns miner.ealtsub)


;; Longest Alternating Substring
;; 
;; Write a function that takes a string of digits and returns the longest substring that contains only alternating odd/even digits. If two substrings have the same length, return the one that appears first.
;; 


(longest-alt-subs "") ;=> ""
(longest-alt-subs "1") ;=> "1"
(longest-alt-subs "123") ;=> "123"
(longest-alt-subs "13") ;=> "1"
(longest-alt-subs "122381") ;=> "2381"
