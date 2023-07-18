(ns letsplotdemo.core
  (:require [letsplotdemo.signatures :as sigs]
            [seesaw.core :as gui])
  (:import  [jetbrains.datalore.plot MonolithicCommon]
            [jetbrains.datalore.vis.swing.batik DefaultPlotPanelBatik]
            [org.jetbrains.letsPlot.geom geomDensity geomHistogram]
            [org.jetbrains.letsPlot.intern Plot Feature #_toSpec]
            [org.jetbrains.letsPlot LetsPlot]
            [java.awt Dimension GridLayout]
            [org.jetbrains.letsPlot Stat$density Stat$bin]
            [org.jetbrains.letsPlot.pos PosKt]
            [org.jetbrains.letsPlot GgplotKt]
            [org.jetbrains.letsPlot.intern.layer PosOptions]
            [org.jetbrains.letsPlot.intern ToSpecConvertersKt]
            [javax.swing JPanel JFrame]))

;;package org.jetbrains.letsPlot.pos pos.kt
(def positionIdentity (PosKt/getPositionIdentity))
(defn positionDodge ^PosOptions [^Number width]
  (PosKt/positionDodge width))
(defn positionDodgeV ^PosOptions [^Number height]
  (PosKt/positionDodgeV height))
(defn positionStack ^PosOptions [& [vjust mode]] (PosKt/positionStack vjust mode))
(defn positionFill ^PosOptions [& [vjust mode]] (PosKt/positionFill vjust mode))
(defn positionJitterDodge ^PosOptions [& [dodgeWidth jitterWith jitterHeight]]
  (PosKt/positionJitterDodge dodgeWidth jitterWith jitterHeight))

;;will complain about reflection..
(defn toSpec [obj] (ToSpecConvertersKt/toSpec obj))


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

(defmacro new-partial [class kvps]
  (let [fullclass  (ns-resolve *ns* class)
        order  (or (get sigs/geom-args  (.getName ^Class fullclass))
                   (throw (ex-info "unknown kotlin class!" {:in class :name (name class)})))
        knowns (reduce-kv (fn [acc k init]
                            (let [idx (order (name k))]
                              (assoc acc idx init))) {} kvps)]
    `(new ~class ~@(map knowns (range (count order))))))

;;define a partially applied kotlin class constructor lol.
(defmacro defpartial [ctor class defaults]
  (let [kvps (gensym "kvps")
        merged (gensym "merged")]
    `(defmacro ~ctor [~kvps]
       (let [~merged (merge  '~defaults ~kvps)]
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
  {stat           (Stat$density.)
   position       positionIdentity
   showLegend     true
   mapping ignore})

(defpartial ->geomHistogram geomHistogram
  {stat           (Stat$bin.)
   position       (positionStack)
   showLegend     true
   mapping ignore})


;;Actual port of the example:

;; fun main() {
;;     val rand = java.util.Random()
;;     val n = 200
;;     val data = mapOf<String, Any>(
;;         "x" to List(n) { rand.nextGaussian() }
;;     )
;;     val plots = mapOf(
;;         "Density" to letsPlot(data) + geomDensity(
;;             color = "dark-green",
;;             fill = "green",
;;             alpha = .3,
;;             size = 2.0
;;         ) { x = "x" },
;;         "Count" to letsPlot(data) + geomHistogram(
;;             color = "dark-green",
;;             fill = "green",
;;             alpha = .3,
;;             size = 2.0
;;         ) { x = "x" },
;;         )


(def ^java.util.Random randgen (java.util.Random.))
(def n 200)
(def data {"x" (vec (repeatedly n #(.nextGaussian randgen)))})


(def plots {"Density" (p+ (letsPlot data)
                          (->geomDensity
                           {color "dark-green"
                            fill  "green"
                            alpha  0.3
                            size   2.0
                            mapping (kfn [obj] (with obj {x "x"}))}))

            "Count" (p+ (letsPlot data)
                        (->geomHistogram
                         {color "dark-green"
                          fill  "green"
                          alpha  0.3
                          size   2.0
                          mapping (kfn [obj] (with obj {x "x"}))}))})

;;ignoring most of the Swing csrap; it's awful in kotlin or java.

;; fun createPlotPanel(): JPanel {
;;                                val rawSpec = plots[plotKey]!!.toSpec()
;;                                val processedSpec = MonolithicCommon.processRawSpecs(rawSpec, frontendOnly = false)

;;                                return DefaultPlotPanelBatik(
;;                                                             processedSpec = processedSpec,
;;                                                             preserveAspectRatio = preserveAspectRadio,
;;                                                             preferredSizeFromPlot = false,
;;                                                             repaintDelay = 10,
;;                                                             ) { messages ->
;;                                                                for (message in messages) {
;;                                                                                           println("[Example App] $message")
;;                                                                                           }
;;                                                                }

;;WEIRD!
;;kotlin exposes what looks like a static class, but it's not.  There's a static instance in INSTANCE,
;;and all the functions (methods) have an implicit first arg to this class that you have to pass...wtf.
(defn process-spec [raw-spec] (.processRawSpecs MonolithicCommon/INSTANCE raw-spec false))

#_(@NotNull final Map<String, Object> processedSpec, final boolean preserveAspectRatio, final boolean preferredSizeFromPlot, final int repaintDelay, @NotNull final Function1<? super List<String>, Unit> computationMessagesHandler)

(defn create-plot-panel ^JPanel [plot]
  (let [spec (toSpec plot)
        processed (process-spec spec)]
    (DefaultPlotPanelBatik. processed true false 10 (kfn [messages] nil))))

(gui/native!)
(defn show-them []
  (gui/invoke-later
   (-> (gui/frame :title "Batik Rendering Let's Plot!" :content
                  (gui/horizontal-panel :items [(create-plot-panel (plots "Density"))
                                               (create-plot-panel (plots "Count"))]))
       gui/pack!
       gui/show!)))



;;Ignored controll /swing crap.
;; private class Controller(
;;     private val plots: Map<String, Plot>,
;;     initialPlotKey: String,
;;     initialPreserveAspectRadio: Boolean
;; ) {
;;     var plotContainerPanel: JPanel? = null
;;     var plotKey: String = initialPlotKey
;;         set(value) {
;;             field = value
;;             rebuildPlotComponent()
;;         }
;;     var preserveAspectRadio: Boolean = initialPreserveAspectRadio
;;         set(value) {
;;             field = value
;;             rebuildPlotComponent()
;;         }

;;     fun rebuildPlotComponent() {
;;         plotContainerPanel?.let {
;;             val container = plotContainerPanel!!
;;             // cleanup
;;             for (component in container.components) {
;;                 if (component is Disposable) {
;;                     component.dispose()
;;                 }
;;             }
;;             container.removeAll()

;;             // build
;;             container.add(createPlotPanel())
;;             container.revalidate()
;;         }
;;     }

;;     fun createPlotPanel(): JPanel {
;;         val rawSpec = plots[plotKey]!!.toSpec()
;;         val processedSpec = MonolithicCommon.processRawSpecs(rawSpec, frontendOnly = false)

;;         return DefaultPlotPanelBatik(
;;             processedSpec = processedSpec,
;;             preserveAspectRatio = preserveAspectRadio,
;;             preferredSizeFromPlot = false,
;;             repaintDelay = 10,
;;         ) { messages ->
;;             for (message in messages) {
;;                 println("[Example App] $message")
;;             }
;;         }
;;     }
;; }
