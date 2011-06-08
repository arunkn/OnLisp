(ns on_lisp.utility)
(require 'swank.core)

(defn find-first [xs func]
  (if-not xs
    nil
    (let [val (func (first xs))]
      (if val
       (list (first xs) val)
       (recur (next xs) func)))))

(defn single? [lst]
  (and (first lst) (not (next lst))))

(defn append1 [lst obj]
  (concat lst (list obj)))

(defn mklist [obj]
  (if (list? obj)
    obj
    (list obj)))

(defn longer [lst1 lst2]
  (cond (empty? lst1) false
	(empty? lst2) true
	:else (recur (rest lst1) (rest lst2))))

(defmacro if-empty? 
  ([lst then else] 
     `(if (empty? ~lst) ~then ~else))
  ([lst then] 
     `(if (empty? ~lst) ~then)))

;; (defmacro if-pred?
;;   ([pred lst then] `(if (~pred ~lst) ~then nil))
;;   ([pred lst then else] `(if (~pred ~lst) ~then ~else)))

;; (defmacro another-if-empty? [lst then & else]
;;   `(if-pred? empty? ~lst ~then ~else))

(defmacro if-nil? 
  ([lst then else] 
     `(if (nil? ~lst) ~then ~else))
  ([lst then] 
     `(if (nil? ~lst) ~then)))

(defn first-conj [lst1 lst2]
  (conj lst1 (first lst2)))

(defn my-filter [func mylst]
  (letfn [(do-filter [lst acc]
		     (if-empty? lst
				(reverse acc)
				(recur (rest lst) (if (func (first lst))
						    (first-conj acc lst)
						    acc))))]
    (do-filter mylst ())))

(defn my-partition [lst n]
  (letfn
      [(rec [source acc]
	    (let [rem (nthnext source n)]
	      (if-nil? rem
		       (reverse (conj acc (take n source)))
		       (recur rem (conj acc (take n source))))))]
    (rec lst ())))

(defn first-coll? [coll] (coll? (first coll)))

(defn my-flatten [mytree]
  (letfn
      [(rec [tree acc]
	    (cond (empty? tree) (reverse acc)
		  (first-coll? tree) (concat acc
					     (rec (first tree) ())
					     (rec (rest tree) ()))
		  :else (recur (rest tree) (conj acc (first tree)))))]
    (rec mytree ())))

(defn prepend [obj lst]
  (concat (list obj) lst))

(defn prune [test tree]
  (letfn
      [(rec [tree acc]
	    (cond
	     (empty? tree) (reverse acc)
	     (first-coll? tree) (recur (rest tree)
				       (prepend (rec (first tree) nil)
						acc))
	     :else (rec (rest tree)
			(if (test (first tree)) acc (prepend (first tree) acc)))))]
    (rec tree ())))

(defn before [lst x y & {:keys [test] :or {test =}}]
)