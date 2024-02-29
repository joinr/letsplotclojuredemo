(ns letsplotdemo.view
  (:require [seesaw.core :as gui]
            [letsplotdemo.core :as lp])
  (:import  [jetbrains.datalore.vis.swing.batik DefaultPlotPanelBatik]
            [org.jetbrains.letsPlot.intern ToSpecConvertersKt]
            [jetbrains.datalore.plot MonolithicCommon]
            [javax.swing JPanel JFrame]))

(defn toSpec [obj] (ToSpecConvertersKt/toSpec obj))
;;WEIRD!
;;kotlin exposes what looks like a static class, but it's not.  There's a static instance in INSTANCE,
;;and all the functions (methods) have an implicit first arg to this class that you have to pass...wtf.
(defn process-spec [raw-spec] (.processRawSpecs MonolithicCommon/INSTANCE raw-spec false))


(defn create-plot-panel ^JPanel [plot]
  (let [spec (toSpec plot)
        processed (process-spec spec)]
    (DefaultPlotPanelBatik. processed true false 10 (lp/kfn [messages] nil))))

(gui/native!)

;;tbd...
(defn show! [plot] )

(defn show-them [plots]
  (gui/invoke-later
   (-> (gui/frame :title "Batik Rendering Let's Plot!" :content
                  (gui/horizontal-panel :items (mapv create-plot-panel plots)))
       gui/pack!
       gui/show!)))
