(ns circuitos.core
  (:require [clojure.string :as string]
            [clojure.tools.macro :as mac]))

(defn clock
  [pulse time]
  (do
    (Thread/sleep time)
    (not pulse)))

(defn d-ff
  [pulse d]
  (when pulse d))

(defn b-i
  [b]
  (if b 1 0))

(defmacro ret-bindings
  [bindings]
  (let [s (gensym "symbol")
        b (gensym "binding")]
      `(let ~bindings     
         (for [[~s ~b] (array-map ~@bindings)]
           ~s))))

(defmacro apply-recur
  [list]
  `(recur ~@(into '() list)))

;; (defmacro recur-let
;;   [bindings]
;;   `(apply-recur
;;     ~(let [s (gensym "symbol")
;;            b (gensym "binding")]
;;       `(let ~bindings     
;;          (for [[~s ~b] (array-map ~@bindings)]
;;            ~s))))

(defmacro loop-let
  [lp-bindings
   lt-bindings
   & code]
  (let [parted-lt-b
        (partition 2 lt-bindings)
        atom-bindings
        (reduce
         (fn [acc [a b]]
           (conj acc a `(atom ~b)))
         []
         parted-lt-b)
        a (gensym "atom")
        b (gensym "binding")]
    `(let ~(into [] (concat lp-bindings
                            atom-bindings))
       (loop ~lp-bindings
         (doseq
             [[~a ~b] '[~@parted-lt-b]]
           (reset! (eval ~a) (eval ~b)))
         ~@code))))

;; (loop [n 0
;;        pulse false
;;        q1 false
;;        q2 false
;;        q3 false
;;        q4 false]
;;   (do
;;     (println (Integer/parseInt (str (b-i q1)
;;                                     (b-i q2)
;;                                     (b-i q3)
;;                                     (b-i q4))
;;                                2))
;;     (apply-recur
;;      (ret-bindings
;;       [n (inc n)
;;        pulse (clock pulse 1000)
;;        q1 (d-ff pulse (not q1))
;;        q2 (d-ff (not q1) (not q2))
;;        q3 (d-ff (not q2) (not q3))
;;        q4 (d-ff (not q3) (not q4))]))))
