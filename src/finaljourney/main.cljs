(ns finaljourney.main
  (:require [enfocus.core :as ef])
  (:use [singult.core :only [render]])
  (:require-macros [enfocus.macros :as em]))

(defn log [& messages]
  (when js/console
    (if (= 1 (count messages))
      (.log js/console (first messages))
      (.log js/console (apply str messages)))))

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
   (string? x) x
   (keyword? x) (name x)
   (map? x) (.-strobj (reduce (fn [m [k v]]
                                (assoc m (clj->js k) (clj->js v))) {} x))
   (coll? x) (apply array (map clj->js x))
   :else x))

(def data (atom {}))

(defn make-stage []
  (js/Kinetic.Stage. (clj->js {:container "container"
                               :width (@data :screen-width)
                               :height (@data :screen-height)})))

(defn make-layer []
  (js/Kinetic.Layer.))

(defn make-rect! [layer rect]
  (let [world (@data :world)
        {x :x
         y :y
         w :width
         h :height} rect
        krect (js/Kinetic.Rect. (clj->js {:x x
                                          :y y
                                          :width w
                                          :height h
                                          ;;:fill "none"
                                          :stroke "black"
                                          :strokeWidth 2
                                          }))
        brect (.createEntity world (clj->js {:x (+ x (/ w 2))
                                             :y (+ y (/ h 2))
                                             :shape "square"
                                             :width w
                                             :height h
                                             :density 10
                                             :color "red"
                                             :draw (fn [])
                                             :onTick (fn []
                                                       (this-as brect
                                                                (let [{x "x"
                                                                       y "y"} (js->clj (.position brect))]
                                                                  (.setPosition krect
                                                                                (- x (/ w 2))
                                                                                (- y (/ h 2))))))
                                             }))]
    (.add layer krect)
    ;;(.draw layer)
    {:kinetic krect :boxbox brect}))

(defn make-boxbox [canvas]
  (let [world (.createWorld js/boxbox canvas (clj->js {:scale 1
                                                       :gravity {:x -10 :y 0}}))]
    (swap! data (fn [data]
                  (assoc data :world world)))
    world))

(defn get-canvas []
  (-> (.getElementById js/document "container")
      (.-childNodes)
      (aget 0)
      (.-childNodes)
      (aget 0)))

(defn get-main-layer []
  (@data :main-layer))

(defn setup-screen []
  (let [screen-width (.-innerWidth js/window)
        screen-height (.-innerHeight js/window)]
    (swap! data (fn [data]
                  (assoc data :screen-width screen-width :screen-height screen-height)))))

(defn init-player! []
  (let [layer (get-main-layer)
        {w :screen-width
         h :screen-height} @data
        x 200
        y (/ h 2)
        rect (make-rect! layer {:x x :y y :width 10 :height 10})]
    (swap! data (fn [data] (assoc data :player {:object rect})))))

(defn setup-world []
  (let [stage (make-stage)
        layer (make-layer)]
    (.add stage layer)
    (swap! data (fn [data]
                  (assoc data :main-layer layer)))
    (let [canvas (get-canvas)
          world (make-boxbox canvas)]
      (.onRender world (fn [] (.draw stage))))
    (init-player!)))

(defn to-degrees [radians]
  (/ (* 180.0 radians) Math/PI))

(defn get-position [object]
  (js->clj (.position (get object :boxbox))))

(defn impulse [object power angle]
  (.applyImpulse (get object :boxbox) power (+ 90 (to-degrees angle))))

(defn thrust [tx ty]
  (let [player-object (get-in @data [:player :object])
        {px "x" py "y"} (get-position player-object)
        dx (- tx px)
        dy (- ty py)
        a (Math/atan2 dy dx)]
    (impulse player-object 100000 a)))


(defn mouse-click-action [event]
  (thrust (.-clientX event) (.-clientY event)))

(em/defaction setup-mouse-events []
  ["canvas"] (em/listen :click mouse-click-action))

(defn setup-controls []
  (setup-mouse-events))

(defn startup []
  (log "startup")
  (setup-screen)
  (setup-world)
  (setup-controls)
  (log "startup done"))
