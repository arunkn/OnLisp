(ns OnLisp.macros
  (:use [OnLisp.utility]))
(require 'swank.core)

;; Final Usage
;; (dolist (x '(a b c))
;; 	(println x))
(defmacro dolist [[v lst & [result]] & body]
  `(do
     (map
      (fn [~v]
  	~@body)
      ~lst)
     ~result))

;;; Example Usage
;; (when-bind [x 5]
;; 	   (println "x:" x))
(defmacro when-bind [[v exp] & body]
  `(let [~v ~exp]
     (when ~v
      ~@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Usage
;; (cl-do [x
;; 	[y 1]
;; 	[z 1 (compute-next z)]]
;;        [end-condition return-value]
;;        body)

;;; Transforms into

;; (loop [x nil
;;        y 1
;;        z 1]
;;   (do
;;     (body-1)
;;     (body-2))
;;   (if (end-condition)
;;       (do
;; 	(return-value))
;;       (recur x y (compute-next z))))
(defn make-initforms [bindforms]
  (->> bindforms
       (mapcat (fn [form]
		 (if (vector? form)
		   (list (first form) (second form))
		   (list form nil)))
	       ,,,)
       (vec)))

(defn make-stepforms [bindforms]
  (->> bindforms
       (map
	(fn [form]
	  (if (vector? form)
	    (if (= (count form) 3)
	      (nth form 2)
	      (nth form 0))
	    form))
	,,,)))

(defmacro cl-do [[& bindings] [test & ret] & body]
  `(loop ~(make-initforms bindings)
     (if ~test
       (if ~ret (do ~@ret))
       (do ~@body
	   (recur ~@(make-stepforms bindings))))))

;; (cl-do
;;  [[y 10]
;;   [x 1 (inc x)]
;;   z]
;;  [(> x 5) x]
;;  (print "x:" x "y:" y)
;;  (println " z:" z))

;; (cl-do [[x (atom 1)]]
;;        [(> @x 10)]
;;        (println "x:" @x)
;;        (swap! x inc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cl-while [test & body]
  `(cl-do []
	  [(not ~test)]
	  ~@body))

;; (let [x (atom 1)]
;;   (cl-while (< @x 10)
;; 	    (println "x:" @x)
;; 	    (swap! x inc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro our-defn [name params & body]
  `(do
     (def ~name (fn ~name ~params
		  ~@body))
     ~name))

;;; Classic macros

(defmacro when-bind [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))


;;; Example usage. The following wont be evaluated because the value of z will
;;; be nil
;; (when-bind* [[x 1]
;; 	     [y (inc x)]
;; 	     [z (odd? (dec x))]]
;; 	    (println "x:" x "y:" y "z:" z))

(defmacro when-bind* [binds & body]
  (if (empty? binds)
    `(do
       ~@body)
    `(let [~@(first binds)]
       (if ~(ffirst binds)
	 (when-bind* ~(rest binds) ~@body)))))


;;; Example usage (with-genyms):
;; (with-genyms (a b c)
;;   (println "hello" a))

(defmacro with-genyms [syms & body]
  (println "syms:" syms)
  `(let ~(vec (mapcat #(list `~% `(gensym)) syms))
     ~@body))


;;; condlet
;;; Usage:
;; (condlet (((= 1 2) (x 'a) (y 'b))
;;           ((= 1 1) (y 'c) (x 'd))
;; 	  (:else   (x 'e) (z 'f)))
;; 	 (list x y z))
;;; The result will be (d c nil)

;;; condlet-binds:
;;; vars:    is a sorted-map with symbol name as key and gensym as value
;;; clauses: a condlet clause
;;; This function should return the list of bindings that should be established for the clause

;;; condlet-clause:
;;; This function should complete the `cond` form.
;;; For the above usage the input to this function will be
;; (((= 1 2) (x 'a) (y 'b))
;;           ((= 1 1) (y 'c) (x 'd))
;; 	  (:else   (x 'e) (z 'f)))
;;; With the above input the function should turn things as correct parameters for cond

(defmacro condlet [clauses & body]
  (letfn [(condlet-binds [vars clause]
			 (vec (mapcat (fn [bindform]
					(if (list? bindform)
					  (list (get vars (first bindform))
						(second bindform))))
				      (rest clause))))

	  (condlet-clause [vars clause bodfn]
	    `(~(first clause) (let ~(conj (vec (interpose nil (vals vars))) nil)
				(let ~(condlet-binds vars clause)
				  (~bodfn ~@(mapcat rest vars))))))]
    (let [bodfn `bodfn#
	  vars (->> clauses
		    (mapcat rest ,,,)
		    (map first ,,,)
		    (distinct ,,,)
		    (reduce (fn [var-map v]
			      (assoc var-map v (gensym)))
			    (sorted-map) ,,,))]
      
      `(letfn [(~bodfn ~(vec (keys vars))
		       ~@body)]
	 (cond ~@(mapcat (fn [clause]
			   (condlet-clause vars  clause bodfn))
			 clauses))))))


;;; Example Usage
;;; (in 5 1 2 3 4) => false
;;; (in 5 1 2 3 4 5) => true
(defmacro in [obj & choices]
  (let [insym `insym#]
    `(let [~insym ~obj]
       (or ~@(map (fn [c] `(= ~insym ~c))
		  choices)))))


;;; Example usage
;; (inq 'a b c d e) => false
;; (inq 'a a b c d e) => true
(defmacro inq [obj & args]
  `(in ~obj ~@(map (fn [a] `'~a) args)))


;;; Example usage
;; (in-if #(< 10 %) 1 2 3 4 5) => false
;; (in-if #(< 10 %) 1 2 3 4 5 20) => true
(defmacro in-if [func & choices]
  (let [fnsym `fnsym#]
    `(let [~fnsym ~func]
       (or ~@(map (fn [choice] `(~fnsym ~choice))
		  choices)))))


(defmacro >case [expr & clauses]
  (letfn [(>casex [g cl]
		  (let [key (first cl)
			r (rest cl)]
		    (cond (list? key) `((in ~g ~@key) ~@r)
			  (= key :else) `(:else ~@r)
			  :else (throw (IllegalArgumentException. (str "Unknown condition"
								       key))))))]
   (let [g (gensym)]
     `(let [~g ~expr]
	(cond ~@(map #(>casex g %)
		     clauses))))))

(let [lst '(1 2 3)]
  (>case (first lst)
	 (5      (println "5"))
	 ("Arun" (println "Arun"))
	 (:else "Unknown")))