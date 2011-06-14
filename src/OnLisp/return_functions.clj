(ns OnLisp.return_functions
  (:use [OnLisp.utility]))

(require 'swank.core)

(defn memoize-func [func]
  (let [cache (atom {})]
    (fn momoized-func [& args]
      (if (contains? @cache args)
	(@cache args)
	(let [new-ele (apply func args)]
	  (swap! cache assoc args new-ele)
	  new-ele)))))

(defn fif [IF THEN & ELSE]
  (fn [x]
    (if (IF x)
      (THEN x)
      (if ELSE
	((first ELSE) x)))))

(defn slave [x]
  (println "Slave:" x))

(defn owner [x]
  (println "Owner:" x))

(defn employer [x]
  (println "Employer:" x))

(defn fint [func & funcs]
  (if-not funcs
    func
    (let [chain (apply fint funcs)]
      #(and (func %)
	    (chain %)))))

(defn fun [func & funcs]
  (if-not funcs
    func
    (let [chain (apply fun funcs)]
      #(or (func %)
	   (chain %)))))

;;; Example usage of lrec
(defn lrec [combiner & [base]]
  (letfn [(self [lst]
		(if-empty? lst
		  (if (fn? base)
		    (base)
		    base)
		  (combiner (first lst)
		       #(self (rest lst)))))]
    self))

;;; LENGTH
;;; (lrec (fn length [x f] (inc (f))) 0)

;;; ALL ELEMENTS ARE ODD
;;; (lrec (fn all-odd? [x f] (and (odd? x) (f))) true)

;;; copy-list
;;; ((lrec (fn [x f] (conj (f) x))) (range 10))

;;; remove-duplicates
;;; ((lrec (fn [x f] (conj (f) x)) #{}) (concat (range 10) (range 5)))

;;; find-if
;;; ((lrec (fn [x f] (if (check-if-ok x) x (f)))) (range 10))

;;; some
;; ((lrec (fn [x f] (or (check-if-ok x) (f)))) (range 10))

(defn our-copy-tree [tree]
  (if (atom? tree)
    tree
    (conj (if (next tree)
	    (our-copy-tree (next tree)))
	  (our-copy-tree (first tree)))))

;;; (count-leaves ()) will be 2 because there are 2 nils hiding
(defn count-leaves [tree]
  (if (atom? tree)
    1
    (+ (count-leaves (first tree))
       (if (next tree)
	 (count-leaves (next tree))
	 1))))

(defn flatten [tree]
  (if (atom? tree)
    (mklist tree)
    (concat (flatten (first tree))
	    (if (next tree)
	      (flatten (next tree))))))

(defn tree-find [tree func]
  (if (atom? tree)
    (and tree (func tree))
    (or (tree-find (first tree) func)
	(if (next tree)
	  (tree-find (next tree) func)))))

(defn tree-trav [combiner & [base]]
  (let [base (or base identity)]
      (letfn [(self [tree]
		    (if (atom? tree)
		      (if (fn? base)
			(base tree)
			base)
		      (combiner (self (first tree))
				(if (next tree)
				  (self (next tree))))))]
	self)))

;;; Copy tree
;;; ((tree-trav conj) '((1 2 3) (4 (5 6) 7 (8)) 9))

;;; count-leaves
;;; ((tree-trav (fn [l r] (+ l (or r 1))) 1) '((1 2 3) (4 (5 6) 7 (8)) 9))

;;; flatten
;;; ((tree-trav concat mklist) '((1 2 3) (4 (5 6) 7 (8)) 9))

(defn tree-rec [combiner & [base]]
  (let [base (or base identity)]
    (letfn [(self [tree]
		  (if (atom? tree)
		    (if (fn? base)
		      (base tree)
		      base)
		    (combiner tree
			      (fn left-process []
				(self (first tree)))
			      (fn right-process []
				(if (next tree)
				  (self (next tree)))))))]
      self)))

;;; Flattne fith tree-rec
;;; ((tree-rec (fn [o l r] (concat (l) (r))) mklist) '((1 2 3) (4 (5 6) 7 (8)) 9))

;;; tree-find
;;; ((tree-rec (fn [o l r] (or (l) (r)))
;;;	   (fn [tree] (and (odd? tree) tree))) (range 10))