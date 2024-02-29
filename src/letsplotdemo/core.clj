(ns letsplotdemo.core
  (:require [letsplotdemo.signatures :as sigs])
  (:import  [org.jetbrains.letsPlot.geom geomDensity geomHistogram geomSegment]
            [org.jetbrains.letsPlot.intern Plot Feature #_toSpec]
            [org.jetbrains.letsPlot LetsPlot]
            [org.jetbrains.letsPlot Stat$density Stat$bin]
            [org.jetbrains.letsPlot.pos PosKt]
            [org.jetbrains.letsPlot GgplotKt]
            [org.jetbrains.letsPlot.intern.layer PosOptions]
            [javax.swing JPanel JFrame]))

;;package org.jetbrains.letsPlot.pos pos.kt
(def positionIdentity (PosKt/getPositionIdentity))
(def statIdentity     (.getIdentity org.jetbrains.letsPlot.Stat/INSTANCE))
(defn positionDodge ^PosOptions [^Number width]
  (PosKt/positionDodge width))
(defn positionDodgeV ^PosOptions [^Number height]
  (PosKt/positionDodgeV height))
(defn positionStack ^PosOptions [& [vjust mode]] (PosKt/positionStack vjust mode))
(defn positionFill ^PosOptions [& [vjust mode]] (PosKt/positionFill vjust mode))
(defn positionJitterDodge ^PosOptions [& [dodgeWidth jitterWith jitterHeight]]
  (PosKt/positionJitterDodge dodgeWidth jitterWith jitterHeight))


;;in kotlin, the function in org.jetbrains.letsPlot letsPlot is
;;defined in the file ggplot.kt....so this gets compiled into a
;;static class with a member:

;;org.jetbrains.letsPlot.GgplotKt/letsPlot
;;weird...

#_
(org.jetbrains.letsPlot.GgplotKt/letsPlot data)

;;for the single arity version lol....

;;for some reason, the examples allow a single arity
;;usage of letsPlot, with the second function
;;parameter elided.  The interop version
;;makes no allowances; a single arg function
;;specifically a   kotlin.jvm.functions.Function1
;;implementation has to be provided.  Its purpose
;;is unknown, but in practice it is applied to the
;;aesthetics object prior to "sealing" [whatever
;;that means].

;;I assume this is to setup the aesthetics mapping
;;somehow.  Weird!  For now we just treat it as
;;an unimportant side effect.


(defn  fn1 ^kotlin.jvm.functions.Function1 [f]
  (reify kotlin.jvm.functions.Function1
    (invoke [this x] (f x))))

(defmacro kfn [args & body]
  `(let [f# (fn ~args ~@body)]
     (fn1 f#)))

(defn as-kfn ^kotlin.jvm.functions.Function1  [f]
  (cond (instance? kotlin.jvm.functions.Function1 f)
        f
        (fn? f) (kfn [x] (f x))
        :else (throw (ex-info "uncoercable kotlin function arg!" {:in f :type (type f)}))))

;;convenience wrapper for the static method (formerly kotlin function).  These will
;;probably be a running theme...
(defn letsPlot
  ([^java.util.Map data alter-mapping]
   (GgplotKt/letsPlot data (as-kfn alter-mapping)))
  ([data] (letsPlot data (kfn [arg] nil))))

;;implements the overloaded + operator, for ggplot stuff.
(defn p+
  ([^Plot l ^Feature r] (.plus l r))
  ([^Plot l x & xs]
   (reduce (fn [^Plot acc ^Feature x]
             (.plus acc x)) l (cons x xs))))

;;can't use reflection to map argnames -> positional args...
;;so kotlin's use of named args for classes has to be done manually (ugh....)

;;there's an implicit `this` for kotlin's
;;receiver types in function literals.


;;need to define a means to actually alter the mapping to the user specified
;;values...
;;https://github.com/JetBrains/lets-plot-kotlin/blob/a6086fe5afbb987645f25db70a42bfe486f28d33/plot-api/src/commonMain/kotlin/org/jetbrains/letsPlot/intern/GenericAesMapping.kt

;;need to scrape meta to avoid reflection warnings...
(defmacro with [obj kvps]
  `(let [obj# ~obj]
     (doto obj#
       ~@(for [[fld v] kvps]
           `(~(symbol (str ".set" (clojure.string/capitalize (name fld))))
             ~v)))))

(defn resolve-default [class]
  (let [res  (or (ns-resolve *ns* class)
                 (ns-resolve (find-ns 'letsplotdemo.core) class)
                 (throw (ex-info "could not resolve class/alias in current ns or letsplotdemo.core" {:in class :ns *ns*})))]
    (cond (class? res)
          res
          (var? res)
          (let [m (meta res)]
            (symbol (-> m :ns ns-name name) (-> m :name name)))
          :else
          res)))

(defmacro new-partial [class kvps]
  (let [fullclass  (resolve-default class)
        order  (or (get sigs/geom-args  (.getName ^Class fullclass))
                   (throw (ex-info "unknown kotlin class!" {:in class :name (name class)})))
        knowns (reduce-kv (fn [acc k init]
                            (let [idx (order (name k))]
                              (assoc acc idx init))) {} kvps)]
    `(new ~fullclass ~@(map knowns (range (count order))))))

;;define a partially applied kotlin class constructor lol.
;;need to lift class ctors...

(defn spop [s]
  (subs s 0 (dec (count s))))

;;allows us to invoke macros from other namespaces and get the
;;alias ctor classess here.  We could also define a simple
;;interpreter for the known classes too, like :stats/density ->
;;Stats$density...TBD.

(defn lift-symbols [m]
  (reduce-kv (fn [acc k expr]
               (cond
                 (= expr 'ignore)
                 acc

                 (list? expr)
                 (let [symb (first expr)]
                   (if-let [cls (when (clojure.string/ends-with? (name symb) ".")
                                  (-> symb name spop symbol))]
                     (assoc acc k (cons (symbol (str (.getName ^Class (resolve-default cls)) ".")) (rest expr)))
                     (assoc acc k (cons (resolve-default symb) (rest expr)))))

                 (symbol? expr) ;;could be a class....
                 (assoc acc k (resolve-default expr))
                 :else
                 acc)) m m))

(defmacro defpartial [ctor class defaults]
  (let [kvps (gensym "kvps")
        merged (gensym "merged")]
    `(defmacro ~ctor [~kvps]
       (let [~merged (merge  (lift-symbols '~defaults) (lift-symbols ~kvps))]
         `(new-partial ~'~class ~~merged)))))

;;defines a kotlin class wrapper with defaulst values. we have to transcribe a
;;few of these... so the idea is that we get defaults at compile time. in the
;;case of geomDensity, we have

;;stat: StatOptions = Stat.density(),
;;position: PosOptions = positionIdentity,
;;showLegend: Boolean = true,
;;mapping: DensityMapping.() -> Unit = {}

;;as non-null default values.

;;so something like
(defpartial ->geomDensity  geomDensity
  {stat           (Stat$density.)  ;;need to infer Stat from letsplotcore.demo or current ns.
   position       positionIdentity
   showLegend     true
   mapping ignore})

(defpartial ->geomHistogram geomHistogram
  {stat           (Stat$bin.)
   position       (positionStack)
   showLegend     true
   mapping ignore})


(defpartial ->geomSegment geomSegment
  {stat           statIdentity
   position       positionIdentity
   showLegend     true
   mapping ignore})
