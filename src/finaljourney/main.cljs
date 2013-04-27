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
  (js/Kinetic.Layer. (clj->js {:width (@data :screen-width)
                               :height (@data :screen-height)})))

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
                                                                (when brect
                                                                  (let [{x "x"
                                                                         y "y"} (js->clj (.position brect))]
                                                                    (.setPosition krect
                                                                                  (- x (/ w 2))
                                                                                  (- y (/ h 2)))
                                                                    ))))
                                             }))]
    (.add layer krect)
    {:kinetic krect :boxbox brect}))

(defn to-boxbox-points [points]
  (vec (for [[x y] (partition 2 points)]
         {:x x :y y})))

(defn make-pattern! [layer]
  (let [world (@data :world)
        w (@data :screen-width)
        h (@data :screen-height)
        krect (js/Kinetic.Polygon. (clj->js {:x 0
                                             :y 0
                                             :points [0 0 w 0 w h 0 h]
                                             :stroke "black"
                                             }))]
    (.add layer krect)
    {:kinetic krect}))

(defn make-poly! [layer x y a points]
  (let [world (@data :world)
        krect (js/Kinetic.Polygon. (clj->js {:x x
                                             :y y
                                             :points points
                                             :fill "white"
                                             :stroke "black"
                                             :strokeWidth 2
                                             :rotationDeg a
                                             }))
        brect (.createEntity world (clj->js {:x x
                                             :y y
                                             :points (to-boxbox-points points)
                                             :shape "polygon"
                                             :density 10
                                             :color "red"
                                             :rotation a
                                             :draw (fn [])
                                             :onTick (fn []
                                                       (this-as t
                                                                (when t
                                                                  (let [{x "x"
                                                                         y "y"} (js->clj (.position t))
                                                                         a (.rotation t)]
                                                                    (.setX krect x)
                                                                    (.setY krect y)
                                                                    (.setRotationDeg krect a)
                                                                    ))))
                                             }))]
    (.add layer krect)
    {:kinetic krect :boxbox brect :points points}))

(defn make-boxbox [canvas]
  (let [world (.createWorld js/boxbox canvas (clj->js {:scale 1
                                                       :gravity {:x -10 :y 0}
                                                       :width (@data :screen-width)
                                                       :height (@data :screen-height)}))]
    (swap! data (fn [data]
                  (assoc data :world world)))
    world))

(defn get-canvas [i]
  (let [i (or i 1)]
    (-> (.getElementById js/document "container")
        (.-childNodes)
        (aget 0)
        (.-childNodes)
        (aget i))))

(defn get-main-layer []
  (@data :main-layer))

(defn get-level-color []
  (let [{x "x" y "y"} (get-position (get-in @data [:player :object]))
        p (get @data :level)
        scale 50000]
    (cond (< p 0) "#000"
          (< p scale) (let [c (int (Math/round (* 255 (- 1.0 (/ (- scale p) scale)))))] (str "rgb(" c "," c "," c ")"))
          :else "#fff")))

(defn setup-screen []
  (let [;;container (.getElementById js/document "container")
        ;;screen-width (.-offsetWidth container)
        ;;screen-height (.-offsetHeight container)
        screen-width (.-innerWidth js/window)
        screen-height (.-innerHeight js/window)
        ;;screen-width (.-clientWidth (.-documentElement js/document))
        ;;screen-height (.-clientHeight (.-documentElement js/document))
        ;;screen-width (.-innerWidth js/window)
        ;;screen-height (.-innerHeight js/window)
        ]
    (log "detected screen " screen-width "x" screen-height)
    (swap! data (fn [data]
                  (assoc data :screen-width screen-width :screen-height screen-height)))))

(defn resize []
  (setup-screen)
  (let [w (@data :screen-width)
        h (@data :screen-height)
        pattern1 (@data :pattern1)
        pattern2 (@data :pattern2)
        pattern3 (@data :pattern3)]
    (.setSize (@data :stage) w h)
    (when pattern
      (.setPoints (pattern1 :kinetic) (clj->js [0 0 w 0 w h 0 h]))
      (.setPoints (pattern2 :kinetic) (clj->js [0 0 w 0 w h 0 h]))
      (.setPoints (pattern3 :kinetic) (clj->js [0 0 w 0 w h 0 h]))))
  (stupid-hack!))

(defn init-player! []
  (let [layer (get-main-layer)
        {w :screen-width
         h :screen-height} @data
        x 200
        y (/ h 2)
        object (make-poly! layer x y 0 [0 0 30 10 0 20])]
    (impulse object 10000000 0)
    (swap! data (fn [data] (assoc data :player {:object object})))))

(defn make-fallen! []
  (let [layer (get-main-layer)
        {sw :screen-width
         sh :screen-height} @data
        x (+ sw (rand-int (max 0 (- sw 250))))
        y (rand-int sh)
        w (+ 5 (rand 10) (rand 20) (rand 30))
        h (+ 5 (rand 10) (rand 20) (rand 30))
        weight (* w h)
        object (make-poly! layer x y (rand 360) [0 0 w 0 w h 0 h])]
    (.density (object :boxbox) weight)
    ;;(log "xy " x " " y)
    (impulse object (+ (rand 200000) (rand 300000)) (rand (* 2 Math/PI)))
    (impulse object (+ 1000000) 3.141)
    (torque object (rand 100000000))
    (swap! data (fn [data] (update-in data [:fallen] conj {:object object})))))

(defn remove-fallen? [fallen]
  (let [{x "x" y "y"} (get-position (fallen :object))]
    (or (< x -50)
        (< y -50)
        (>= y (+ (@data :screen-height) 50)))))

(defn end! []
  (when-not (@data :end?)
    (log "end")
    (em/at js/document ["canvas"] (em/chain (em/fade-out 2000)))
    (em/at js/document [".black"] (em/chain (em/add-class "show")
                                            (em/fade-in 4000)))
    (swap! data (fn [data] (assoc data :end? true)))))

(defn tick []
  (let [removes (filter remove-fallen? (@data :fallen))
        fallen (remove remove-fallen? (@data :fallen))]
    (swap! data (fn [data]
                  (assoc data :fallen fallen)))
    ;; WTF have to put this here otherwise doseq is skipped
    (count fallen)
    (doseq [f removes]
      ;;(log "removing " f)
      (let [{boxbox :boxbox
             kinetic :kinetic} (f :object)]
        (.destroy boxbox)
        (.destroy kinetic)
        (make-fallen!))))
  (let [player-object (get-in @data [:player :object])
        {px "x" py "y"} (get-position player-object)]
    (when (or (< px 0)
              (< py 0)
              (>= py (@data :screen-height)))
      (end!)))
  (let [level (get @data :level 0)
        speed 1]
    (swap! data (fn [data]
                  (update-in data [:level] (fn [x] (+ x speed)))))
    ;;(update-in data [:level] inc)))
    (let [pattern (get-in @data [:pattern1])]
      (when pattern
        (.setFill (pattern :kinetic) (get-level-color))))
    (let [pattern (get-in @data [:pattern2])]
      (when pattern
        (.setFillPatternOffset (pattern :kinetic) (clj->js [(/ level 50) -500]))
        (.setFillPatternScale (pattern :kinetic) (+ 1 (min 30 (Math/abs (- 30 (/ level 300))))))
        (.setFillPatternRotationDeg (pattern :kinetic) (mod (/ level 300) 360))
        (.setOpacity (pattern :kinetic) (max 0 (/ (- 6000 level) 5000)))
        ))
    (let [pattern (get-in @data [:pattern3])]
      (when pattern
        (.setFillPatternOffset (pattern :kinetic) (clj->js [(/ level 77) -500]))
        (.setFillPatternScale (pattern :kinetic) (+ 1 (min 30 (Math/abs (- 30 (/ level 700))))))
        (.setFillPatternRotationDeg (pattern :kinetic) (- 360 (mod (/ level 200) 360)))
        (.setOpacity (pattern :kinetic)
                     (cond (< level 8000) 0
                           (< level 13000) (/ (- level 8000) 5000)
                           (< level 18000) (- 1.0 (/ (- level 13000) 5000))
                           :else 0))
        ))
    ))


(defn stupid-hack! []
  (let [canvas (get-canvas)
        background (get-canvas 0)]
    ;; Stupid hack, why doesn't it get full size in Firefox otherwise?
    (set! (.-width canvas) (@data :screen-width))
    (set! (.-height canvas) (@data :screen-height))
    (set! (.-width background) (@data :screen-width))
    (set! (.-height background) (@data :screen-height))))

(defn setup-world []
  (let [stage (make-stage)
        background (make-layer)
        layer (make-layer)]
    (.add stage background)
    (.add stage layer)
    (swap! data (fn [data]
                  (-> data
                      (assoc :main-layer layer)
                      (assoc :background background)
                      (assoc :stage stage)
                      )))
    (let [image1 (js/Image.)
          image2 (js/Image.)
          pattern1 (make-pattern! background)
          pattern2 (make-pattern! background)
          pattern3 (make-pattern! background)]
      (set! (.-onload image1) (fn []
                                (swap! data (fn [data]
                                              (assoc data :image1 image1)))
                                (.setFillPatternImage (pattern2 :kinetic) (@data :image1))
                                (.setFillPatternOffset (pattern2 :kinetic) [0 0])
                                (.draw background)))
      (set! (.-src image1)  "/img/pattern1.png")
      (set! (.-onload image2) (fn []
                                (swap! data (fn [data]
                                              (assoc data :image2 image2)))
                                (.setFillPatternImage (pattern3 :kinetic) (@data :image2))
                                (.setFillPatternOffset (pattern3 :kinetic) [0 0])
                                (.draw background)))
      (set! (.-src image2)  "/img/pattern2.png")
      (swap! data (fn [data]
                    (-> data
                        (assoc :pattern1 pattern1)
                        (assoc :pattern2 pattern2)
                        (assoc :pattern3 pattern3)))))
    (stupid-hack!)
    (let [canvas (get-canvas)
          world (make-boxbox canvas)]
      (.onRender world (fn [] (.draw stage)))
      (.onTick world tick))
    (init-player!)
    (swap! data (fn [data]
                  (assoc data :fallen [])))
    (doseq [i (range 15)]
      (make-fallen!))))

(defn to-degrees [radians]
  (/ (* 180.0 radians) Math/PI))

(defn get-position [object]
  (js->clj (.position (get object :boxbox))))

(defn impulse [object power angle]
  (.applyImpulse (get object :boxbox) power (+ 90 (to-degrees angle))))

(defn torque [object power]
  (.applyTorque (get object :boxbox) power))

(defn thrust [tx ty]
  (let [player-object (get-in @data [:player :object])
        {px "x" py "y"} (get-position player-object)
        dx (- tx px)
        dy (- ty py)
        a (Math/atan2 dy dx)]
    (impulse player-object 1000000 a)))


(defn mouse-click-action [event]
  (let [player-object (get-in @data [:player :object])
        x (.-clientX event)
        y (.-clientY event)]
    (thrust x y)))

(em/defaction setup-mouse-events []
  ["canvas"] (em/listen :click mouse-click-action))

(defn tap-action [event]
  (let [x (.-x (aget (.-position event) 0))
        y (.-y (aget (.-position event) 0))]
    (thrust x y)))

(defn setup-touch-events []
  (let [hammer (js/Hammer. (get-canvas)
                           (clj->js {:prevent_default true
                                     :tap true
                                     :drag true
                                     }))]
    (set! (.-ontap hammer) tap-action)))


(defn setup-controls []
  (setup-mouse-events)
  (setup-touch-events))

(defn startup []
  (log "startup")
  (setup-screen)
  (setup-world)
  (setup-controls)
  (set! (.-onresize js/window) resize)
  (em/at js/document ["canvas"] (em/chain (em/fade-in 2000)))
  (log "startup done"))
