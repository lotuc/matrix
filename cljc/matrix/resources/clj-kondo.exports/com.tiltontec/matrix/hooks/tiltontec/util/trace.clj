(ns hooks.tiltontec.util.trace)

(defmacro wtrx [[lo hi & trxargs] & body]
  `(do [~lo ~hi (list ~@trxargs)] ~@body))

(defmacro trx [label & vals]
  `(do ~(when (not (nil? label))
          (str label))
       [~@vals]))
