(ns on_lisp.utility)
(require 'swank.core)

(defn find-first-with-val [xs func]
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

(defn exists? [lst obj & {:keys [test] :or {test =}}]
  (when lst
    (if (test (first lst) obj )
      lst
      (recur (next lst) obj test))))


(defn before [lst x y & {:keys [test] :or {test =}}]
  (when lst
    (let [f (first lst)]
      (cond (test x f) lst
            (test y f) false
            :else (recur (rest lst) x y test)))))

(defn after [lst x y & {:keys [test] :or {test =}}]
  (let [rst (before lst y x :test test)]
    (and rst
         (some identity (map #(test x %) rst)))))

(defn duplicate [lst obj & {:keys [test] :or {test =}}]
  (let [pos (exists? lst obj)]
    (and pos
         (exists? (rest pos) obj))))

(defn split-if [lst fn]
  (loop [src lst
         acc ()]
    (when src
      (if (fn (first src))
        [(reverse acc) src]
        (recur (next src) (conj acc (first src)))))))

(defn most [lst fn & {:keys [compare default] :or {compare > default nil}}]
  (letfn [(find-most [lst best-val best-ele]
            (if lst
              (let [cur-ele (first lst)
                    cur-val (fn cur-ele)]
                (if (compare best-val cur-val)
                  (recur (next lst) best-val best-ele)
                  (recur (next lst) cur-val cur-ele)))
              [best-val best-ele]))]
    (if (seq lst)
      (find-most (next lst) (fn (first lst)) (first lst))
      default)))

(defn best [lst fn]
  (if (seq lst)
    (loop [lst lst
          wins (first lst)]
      (if (seq lst)
        (if (fn wins (first lst))
          (recur (next lst) wins)
          (recur (next lst) (first lst)))
        wins))))

(defn most-n [lst fn]
  (if-not (seq lst)
    [nil nil]
    (loop [result (list (first lst))
          most (fn (first lst))
          remaining (next lst)]
      (if-not remaining
        [result most]
        (let [score (fn (first remaining))]
          (cond (< score most) (recur result most (next remaining))
                (> score most) (recur (list (first remaining)) score (next remaining))
                :else (recur (conj  result (first remaining))
                             most
                             (next remaining))))))))

(defn map-a->b [fn a b & {:keys [step] :or {step 1}}]
  (loop [i a
         result ()]
    (if (> i b)
      (reverse result)
      (recur (+ step i) (conj result (fn i))))))

(defn map-0->n [fn n & {:keys [step] :or {step 1}}]
  (map-a->b fn 0 n step))

(defn map-1->n [fn n & {:keys [step] :or {step 1}}]
  (map-a->b fn 1 n step))

(defn map-> [fn start test-fn succ-fn]
  (loop [i start
         result ()]
    (if (test-fn i)
      (reverse result)
      (recur (succ-fn i) (conj result (fn i))))))


