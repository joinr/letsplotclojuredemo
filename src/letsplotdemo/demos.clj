(ns letsplotdemo.demos
  (:require [letsplotdemo.core :as lp
             :refer [letsPlot p+ new-partial defpartial
                     ->geomDensity ->geomHistogram ->geomSegment kfn with]]
            [letsplotdemo.view :as view]
            [seesaw.core :as gui]
            [tablecloth.api :as tc]))

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


(defmacro aes [m]
  `(kfn [obj#] (with obj# ~m)))

(def ^java.util.Random randgen (java.util.Random.))
(def n 200)
(def data {"x" (vec (repeatedly n #(.nextGaussian randgen)))})

(def plots {"Density" (p+ (letsPlot data)
                          (->geomDensity
                           {color "dark-green"
                            fill  "green"
                            alpha  0.3
                            size   2.0
                            mapping (aes {x "x"})}))

            "Count" (p+ (letsPlot data)
                        (->geomHistogram
                         {color "dark-green"
                          fill  "green"
                          alpha  0.3
                          size   2.0
                          mapping (aes {x "x"})}))})


(defn demo1 [] (view/show-them [(plots "Density") (plots "Count")]))

(def iris
  (-> "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/iris.csv"
      (tc/dataset {:key-fn keyword})
      (tc/rename-columns {:Sepal.Length :sepal-length
                          :Sepal.Width :sepal-width
                          :Petal.Length :petal-length
                          :Petal.Width :petal-width
                          :Species :species})
      (tc/rename-columns :all name)))

;;original example
#_
(r/r+ (gg/ggplot (toydata/iris-ds)
                 (gg/aes :x 'sepal_length
                         :y 'petal_length
                         :xend 'sepal_width
                         :yend 'petal_width
                         :color '(factor species))
                 (gg/geom_segment :size 5 :alpha 0.1)
                 (gg/scale_color_brewer :palette "Set1")))

;;currently expecting strings....

(def iris-plot (p+ (letsPlot iris)
                   (->geomSegment
                    {alpha  0.1
                     size   5.0
                     mapping (aes {x "sepal-length"
                                   y "petal-length"
                                   xend "sepal-width"
                                   yend "petal-width"
                                   color "species"})})))
