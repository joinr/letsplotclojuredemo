(ns letsplotdemo.signatures
  (:require [clojure.java.io :as io]))

;;scrape geometry signatures from class names so that
;;we can create wrappers for the function signatures.

;;kotlin allows by-name ctors, but we don't get
;;that through interop and the names are elided
;;in reflection.  So we need to scrape the
;;.kt files to get the class ctor args we want.


(defn class-files []
  (let [root (io/file "../lets-plot-kotlin/plot-api/src/commonMain/kotlin/org/jetbrains/letsPlot/geom/")]
    (->> root
         file-seq
         (filter (fn [f] (and (not (.isDirectory f))
                              (= (.getParentFile f) root)))))))

#_
(def ex
"class geomDensity(
    data: Map<*, *>? = null,
    stat: StatOptions = Stat.density(),
    position: PosOptions = positionIdentity,
    showLegend: Boolean = true,
    sampling: SamplingOptions? = null,
    tooltips: TooltipOptions? = null,
    orientation: String? = null,
    override val x: Number? = null,
    override val y: Number? = null,
    override val alpha: Number? = null,
    override val color: Any? = null,
    override val fill: Any? = null,
    override val linetype: Any? = null,
    override val size: Number? = null,
    override val weight: Any? = null,
    override val bw: Any? = null,
    override val kernel: String? = null,
    override val n: Int? = null,
    override val trim: Boolean? = null,
    override val adjust: Number? = null,
    override val fullScanMax: Int? = null,
    private val quantiles: List<Number>? = null,
    private val quantileLines: Boolean? = null,
    override val colorBy: String? = null,
    override val fillBy: String? = null,
    mapping: DensityMapping.() -> Unit = {}

)")

(defn balance [^String xs]
  (loop [l     1
         depth 1]
  (if (zero? depth)
    l
    (recur  (unchecked-inc l) (case (nth xs l)
                                  \( (unchecked-inc depth)
                                  \) (unchecked-dec depth)
                                  depth)))))

(defn class-def [txt]
  (let [res    (re-find #"class .*" txt)
        chunk  (subs txt (clojure.string/index-of  txt res))
        l      (clojure.string/index-of chunk "(")
        [class args]  [(-> (subs chunk 0 l) (clojure.string/replace "class " ""))
                       (subs chunk l)]]
    {:class class
     :args (->> (subs args 0 (balance (subs args 1)))
                (re-seq #"[a-zA-Z]+:")
                (mapv #(subs % 0 (- (count %) 1)))
                (map-indexed (fn [idx v] [v idx]))
                (apply concat)
                (apply array-map))}))

(defn class-defs [& {:keys [prefix]
                     :or {prefix "org.jetbrains.letsPlot.geom"}}]
  (->> (for [fl (class-files)]
         (-> (class-def (slurp fl))
             (update :class #(str prefix "." %))))
       (into {} (map (juxt :class :args)))))


;;generated from the above..

(def geom-args
{"org.jetbrains.letsPlot.geom.geomArea"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "alpha" 8, "color" 9, "fill" 10, "linetype" 11, "size"
 12, "colorBy" 13, "fillBy" 14, "mapping" 15},
 "org.jetbrains.letsPlot.geom.geomMap"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "map" 6, "mapJoin" 7, "useCRS" 8, "x" 9, "y" 10, "size" 11, "linetype"
 12, "color" 13, "fill" 14, "alpha" 15, "colorBy" 16, "fillBy" 17, "mapping"
 18},


 "org.jetbrains.letsPlot.geom.geomAreaRidges"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "height" 8, "quantile" 9, "alpha" 10, "color" 11, "fill"
 12, "linetype" 13, "size" 14, "weight" 15, "scale" 16, "minHeight"
 17, "quantileLines" 18, "tailsCutoff" 19, "quantiles" 20, "bw" 21, "kernel"
 22, "n" 23, "trim" 24, "adjust" 25, "fullScanMax" 26, "colorBy" 27, "fillBy"
 28, "mapping" 29},


 "org.jetbrains.letsPlot.geom.geomJitter"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "alpha" 8, "color" 9, "fill" 10, "shape" 11, "size"
 12, "stroke" 13, "width" 14, "height" 15, "colorBy" 16, "fillBy" 17, "mapping"
 18},

 "org.jetbrains.letsPlot.geom.geomDensity2DFilled"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "size" 8, "linetype" 9, "color" 10, "fill" 11, "alpha"
 12, "weight" 13, "bw" 14, "kernel" 15, "n" 16, "adjust" 17, "contour"
 18, "bins" 19, "binWidth" 20, "colorBy" 21, "fillBy" 22, "mapping" 23},

 "org.jetbrains.letsPlot.geom.geomContour"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "z" 8, "alpha" 9, "color" 10, "linetype" 11, "size" 12, "bins"
 13, "binWidth" 14, "colorBy" 15, "mapping" 16},

 "org.jetbrains.letsPlot.geom.geomLine" {"data" 0, "stat" 1, "position" 2,
 "showLegend" 3, "sampling" 4, "tooltips" 5, "x" 6, "y" 7, "alpha" 8, "color" 9,
 "linetype" 10, "size" 11, "colorBy" 12, "mapping" 13},

 "org.jetbrains.letsPlot.geom.geomContourFilled"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "z" 8, "size" 9, "linetype" 10, "color" 11, "fill" 12, "alpha"
 13, "bins" 14, "binWidth" 15, "colorBy" 16, "fillBy" 17, "mapping" 18},

 "org.jetbrains.letsPlot.geom.geomQQLine"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "sample" 6, "alpha" 7, "color" 8, "linetype" 9, "size" 10, "distribution"
 11, "dParams" 12, "quantiles" 13, "colorBy" 14, "mapping" 15},

 "org.jetbrains.letsPlot.geom.geomDotplot"

 {"data" 0, "showLegend" 1, "sampling" 2, "tooltips" 3, "x" 4, "bins"
 5, "center" 6, "boundary" 7, "method" 8, "binWidth" 9, "stackSize"
 10, "stackDir" 11, "stackRatio" 12, "dotSize" 13, "stackGroups" 14, "stroke"
 15, "alpha" 16, "color" 17, "fill" 18, "colorBy" 19, "fillBy" 20, "mapping"
 21},

 "org.jetbrains.letsPlot.geom.geomDensity"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "x" 7, "y" 8, "alpha" 9, "color" 10, "fill" 11, "linetype"
 12, "size" 13, "weight" 14, "bw" 15, "kernel" 16, "n" 17, "trim" 18, "adjust"
 19, "fullScanMax" 20, "quantiles" 21, "quantileLines" 22, "colorBy"
  23, "fillBy" 24, "mapping" 25},

 "org.jetbrains.letsPlot.geom.geomBin2D"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "width" 8, "height" 9, "alpha" 10, "color" 11, "fill"
 12, "linetype" 13, "size" 14, "weight" 15, "bins" 16, "binWidth" 17, "drop"
 18, "colorBy" 19, "fillBy" 20, "mapping"
  21},

 "org.jetbrains.letsPlot.geom.geomYDotplot"

 {"data" 0, "showLegend" 1, "sampling" 2, "tooltips" 3, "x" 4, "y" 5, "bins"
 6, "center" 7, "boundary" 8, "method" 9, "binWidth" 10, "stackSize"
 11, "stackDir" 12, "stackRatio" 13, "dotSize" 14, "stackGroups" 15, "stroke"
 16, "alpha" 17, "color" 18, "fill" 19, "colorBy" 20, "fillBy" 21, "mapping"
  22},

 "org.jetbrains.letsPlot.geom.geomVLine"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "xintercept" 7, "alpha" 8, "color" 9, "linetype" 10, "size"
  11, "colorBy" 12, "mapping" 13},

 "org.jetbrains.letsPlot.geom.geomBoxplot"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "x" 7, "y" 8, "lower" 9, "middle" 10, "upper" 11, "ymin"
 12, "ymax" 13, "alpha" 14, "color" 15, "fill" 16, "size" 17, "linetype"
 18, "shape" 19, "width" 20, "weight" 21, "outlierColor" 22, "outlierFill"
 23, "outlierShape" 24, "outlierSize" 25, "outlierStroke" 26, "fatten"
 27, "whiskerWidth" 28, "varWidth" 29, "coef" 30, "colorBy" 31, "fillBy"
 32, "mapping" 33},



 "org.jetbrains.letsPlot.geom.RasterData private constructor"

 {"data" 0, "width" 1, "height" 2, "nChannels" 3},

 "org.jetbrains.letsPlot.geom.geomQQ"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "sample" 6, "alpha" 7, "color" 8, "fill" 9, "shape" 10, "size" 11, "stroke"
 12, "distribution" 13, "dParams" 14, "colorBy" 15, "fillBy" 16, "mapping" 17},


 "org.jetbrains.letsPlot.geom.geomRibbon"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "ymin" 7, "ymax" 8, "size" 9, "linetype" 10, "color" 11, "fill"
 12, "alpha" 13, "colorBy" 14, "fillBy" 15, "mapping" 16},

 "org.jetbrains.letsPlot.geom.geomRaster"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "x" 5, "y"
 6, "alpha" 7, "fill" 8, "fillBy" 9, "mapping" 10},

 "org.jetbrains.letsPlot.geom.geomBar"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "x" 7, "y" 8, "alpha" 9, "color" 10, "fill" 11, "width"
 12, "size" 13, "weight" 14, "colorBy" 15, "fillBy" 16, "mapping" 17},

 "org.jetbrains.letsPlot.geom.geomHistogram"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "x" 7, "y" 8, "alpha" 9, "color" 10, "fill" 11, "size"
 12, "weight" 13, "bins" 14, "binWidth" 15, "center" 16, "boundary"
 17, "colorBy" 18, "fillBy" 19, "mapping"
  20},

 "org.jetbrains.letsPlot.geom.geomFreqpoly"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "x" 7, "y" 8, "alpha" 9, "color" 10, "linetype" 11, "size"
 12, "bins" 13, "binWidth" 14, "center" 15, "boundary" 16, "colorBy"
  17, "mapping" 18},

 "org.jetbrains.letsPlot.geom.geomErrorBar"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "ymin" 7, "ymax" 8, "width" 9, "y" 10, "xmin" 11, "xmax" 12, "height"
 13, "alpha" 14, "color" 15, "linetype" 16, "size" 17, "colorBy" 18, "mapping"
 19},

 "org.jetbrains.letsPlot.geom.geomLineRange"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "ymin" 7, "ymax" 8, "alpha" 9, "color" 10, "linetype" 11, "size"
  12, "colorBy" 13, "mapping" 14},

 "org.jetbrains.letsPlot.geom.geomHLine"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "yintercept" 7, "alpha" 8, "color" 9, "linetype" 10, "size"
  11, "colorBy" 12, "mapping" 13},

 "org.jetbrains.letsPlot.geom.geomText"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "map" 6, "mapJoin" 7, "useCRS" 8, "x" 9, "y" 10, "label" 11, "alpha"
 12, "color" 13, "size" 14, "family" 15, "fontface" 16, "hjust" 17, "vjust"
 18, "angle" 19, "lineheight" 20, "labelFormat" 21, "naText" 22, "nudgeX"
 23, "nudgeY" 24, "sizeUnit" 25, "colorBy" 26, "mapping"
  27},


 "org.jetbrains.letsPlot.geom.geomSmooth"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "x" 7, "y" 8, "ymin" 9, "ymax" 10, "size" 11, "linetype"
 12, "color" 13, "fill" 14, "alpha" 15, "method" 16, "n" 17, "level" 18, "se"
 19, "span" 20, "deg" 21, "seed" 22, "maxN" 23, "colorBy" 24, "fillBy"
  25, "mapping" 26},

 "org.jetbrains.letsPlot.geom.geomPointRange"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "fatten" 6, "x" 7, "y" 8, "ymin" 9, "ymax" 10, "alpha" 11, "color"
 12, "fill" 13, "linetype" 14, "shape" 15, "size" 16, "stroke" 17, "colorBy"
  18, "fillBy" 19, "mapping" 20},


 "org.jetbrains.letsPlot.geom.geomTile"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "width" 8, "height" 9, "alpha" 10, "color" 11, "fill"
 12, "linetype" 13, "size" 14, "colorBy" 15, "fillBy" 16, "mapping"
  17},


 "org.jetbrains.letsPlot.geom.geomLollipop"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "x" 7, "y" 8, "size" 9, "stroke" 10, "linewidth"
 11, "color" 12, "fill" 13, "alpha" 14, "shape" 15, "linetype" 16, "fatten"
 17, "slope" 18, "intercept" 19, "dir" 20, "colorBy" 21, "fillBy" 22, "mapping"
  23},

 "org.jetbrains.letsPlot.geom.geomQQ2Line"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "alpha" 8, "color" 9, "linetype" 10, "size" 11, "quantiles"
  12, "colorBy" 13, "mapping" 14},

 "org.jetbrains.letsPlot.geom.geomPoint"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "map" 6, "mapJoin" 7, "useCRS" 8, "x" 9, "y" 10, "alpha" 11, "color"
 12, "fill" 13, "shape" 14, "size" 15, "stroke" 16, "sizeUnit" 17, "colorBy"
  18, "fillBy" 19, "mapping" 20},

 "org.jetbrains.letsPlot.geom.geomSegment"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "arrow" 6, "x" 7, "y" 8, "xend" 9, "yend" 10, "alpha" 11, "color"
 12, "linetype" 13, "size" 14, "colorBy" 15, "mapping" 16},

 "org.jetbrains.letsPlot.geom.geomLabel"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "map" 6, "mapJoin" 7, "useCRS" 8, "x" 9, "y" 10, "label" 11, "alpha"
 12, "color" 13, "fill" 14, "size" 15, "family" 16, "fontface" 17, "hjust"
 18, "vjust" 19, "angle" 20, "lineheight" 21, "labelFormat" 22, "naText"
 23, "nudgeX" 24, "nudgeY" 25, "labelPadding" 26, "labelR" 27, "labelSize"
 28, "sizeUnit" 29, "colorBy" 30, "fillBy" 31, "mapping" 32},

 "org.jetbrains.letsPlot.geom.geomStep"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "direction"
 5, "x" 6, "y" 7, "alpha" 8, "color" 9, "linetype" 10, "size" 11, "colorBy"
 12, "mapping" 13},

 "org.jetbrains.letsPlot.geom.geomRect"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "map" 6, "mapJoin" 7, "useCRS" 8, "xmin" 9, "xmax" 10, "ymin" 11, "ymax"
 12, "alpha" 13, "color" 14, "linetype" 15, "size" 16, "fill" 17, "colorBy"
 18, "fillBy" 19, "mapping" 20},

 "org.jetbrains.letsPlot.geom.geomCrossbar"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "fatten" 6, "x" 7, "ymin" 8, "ymax" 9, "middle" 10, "width" 11, "alpha"
 12, "color" 13, "fill" 14, "linetype" 15, "size" 16, "colorBy" 17, "fillBy"
 18, "mapping" 19},

 "org.jetbrains.letsPlot.geom.geomQQ2"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "alpha" 8, "color" 9, "fill" 10, "shape" 11, "size"
 12, "stroke" 13, "colorBy" 14, "fillBy" 15, "mapping" 16},

 "org.jetbrains.letsPlot.geom.geomPolygon"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "map" 6, "mapJoin" 7, "useCRS" 8, "x" 9, "y" 10, "size" 11, "linetype"
 12, "color" 13, "fill" 14, "alpha" 15, "colorBy" 16, "fillBy" 17, "mapping"
 18},

 "org.jetbrains.letsPlot.geom.geomPath"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "map" 6, "mapJoin" 7, "useCRS" 8, "x" 9, "y" 10, "alpha" 11, "color"
 12, "linetype" 13, "size" 14, "colorBy" 15, "mapping" 16},

 "org.jetbrains.letsPlot.geom.geomDensity2D"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "x" 6, "y" 7, "z" 8, "alpha" 9, "color" 10, "linetype" 11, "size"
 12, "weight" 13, "bw" 14, "kernel" 15, "n" 16, "adjust" 17, "contour"
 18, "bins" 19, "binWidth" 20, "colorBy" 21, "mapping" 22},

 "org.jetbrains.letsPlot.geom.geomViolin"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "orientation" 6, "quantiles" 7, "showHalf" 8, "quantileLines" 9, "x" 10, "y"
 11, "violinWidth" 12, "alpha" 13, "color" 14, "fill" 15, "linetype" 16, "size"
 17, "width" 18, "weight" 19, "scale" 20, "tailsCutoff" 21, "bw" 22, "kernel"
 23, "n" 24, "trim" 25, "adjust" 26, "fullScanMax" 27, "colorBy" 28, "fillBy"
 29, "mapping" 30},

 "org.jetbrains.letsPlot.geom.geomPie"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "tooltips"
 5, "labels" 6, "map" 7, "mapJoin" 8, "useCRS" 9, "x" 10, "y" 11, "slice"
 12, "explode" 13, "size" 14, "fill" 15, "alpha" 16, "weight" 17, "hole"
 18, "stroke" 19, "strokeColor" 20, "fillBy" 21, "mapping" 22},

 "org.jetbrains.letsPlot.geom.geomABLine"

 {"data" 0, "stat" 1, "position" 2, "showLegend" 3, "sampling" 4, "orientation"
 5, "slope" 6, "intercept" 7, "alpha" 8, "color" 9, "linetype" 10, "size"
 11, "colorBy" 12, "mapping" 13}})
