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

(defn get-canvas []
  (-> (.getElementById js/document "container")
      (.-childNodes)
      (aget 0)
      (.-childNodes)
      (aget 0)))

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
  (let [screen-width (.-innerWidth js/window)
        screen-height (.-innerHeight js/window)]
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
    (impulse object 5000000 0)
    (swap! data (fn [data] (assoc data :player {:object object})))
    (.onImpact (get-in @data [:player :object :boxbox])
               (fn [entity normalForce tangentForce]
                 (let [force (* normalForce 0.0001)
                       force (if (< force 10) 0 force)]
                   ;;(log "impact " force)
                   (swap! data (fn [data]
                                 (update-in data [:player :disabled] + force))))))))

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
    (when-let [b (object :boxbox)]
      (.density b weight))
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

(defn signum [x]
  (if (< x 0) -1 1))

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
        (when boxbox
          (.destroy boxbox))
        (when kinetic
          (.destroy kinetic))
        (make-fallen!))))
  (let [player-object (get-in @data [:player :object])
        disabled (get-in @data [:player :disabled] 0)
        a (get-heading player-object)]
    (if (> disabled 0)
      (do
        (.setFill (player-object :kinetic) "#aaa")
        (swap! data (fn [data]
                      (update-in data [:player :disabled] - 1))))
      (let [ta (/ (* 180.0 (get-in @data [:player :target-angle] 0)) Math/PI)
            ta (proper-angle ta)
            da (- ta a)
            da (if (> da 180) (- da 360) da)
            da (if (< da -180) (+ 360 da) da)
            da (if (and (< ta a) (> (- ta a) 180)) (- da) da)
            te (get-in @data [:player :target-error] 0)
            p (* 0.1 da)
            i (* 0.001 te)
            d (* 1 (- da (get-in @data [:player :target-error-last] 0)))
            ]
        (.setFill (player-object :kinetic) "#fff")
        (when (< (Math/abs da) 20)
          (when-let [thrust (get-in @data [:player :thrust])]
            (impulse player-object (* (thrust :distance 0) 1000) (* Math/PI (/ a 180.0)))))
        ;;(log "a " a " ta " ta " da " da)
        (torque player-object (* (+ p i d) 1000000))
        (swap! data (fn [data] (-> data
                                   (update-in [:player :target-error] + da)
                                   (assoc-in [:player :target-error-last] da))))))
    (let [{px "x" py "y"} (get-position player-object)]
      (when (or (< px 0)
                (< py 0)
                (>= py (@data :screen-height)))
        (end!))))
  (let [level (get @data :level 0)
        speed 1]
    (swap! data (fn [data]
                  (update-in data [:level] (fn [x] (+ x speed)))))
    (when (= 0 (mod level 100))
      (em/at js/document ["body"] (em/set-attr :style (str "background-color: " (get-level-color)))))
    ))


(defn stupid-hack! []
  (let [canvas (get-canvas)]
    ;; Stupid hack, why doesn't it get full size in Firefox otherwise?
    (set! (.-width canvas) (@data :screen-width))
    (set! (.-height canvas) (@data :screen-height))))

(defn setup-world []
  (let [stage (make-stage)
        layer (make-layer)]
    (.add stage layer)
    (swap! data (fn [data]
                  (-> data
                      (assoc :main-layer layer)
                      (assoc :stage stage)
                      )))
    (stupid-hack!)
    (let [canvas (get-canvas)
          world (make-boxbox canvas)]
      (.onRender world (fn [] (.draw stage)))
      (.onTick world tick)
      )
    (init-player!)
    (swap! data (fn [data]
                  (assoc data :fallen [])))
    (doseq [i (range 15)]
      (make-fallen!))))

(defn to-degrees [radians]
  (/ (* 180.0 radians) Math/PI))

(defn get-position [object]
  (when-let [b (get object :boxbox)]
    (js->clj (.position b))))

(defn proper-angle [a]
  (loop [a (mod a 360)]
    (if (< a 0)
      (recur (+ 360 a))
      a)))

(defn get-heading [object]
  (when-let [b (get object :boxbox)]
    (let [a (js->clj (.rotation b))]
      (proper-angle a))))

(defn impulse [object power angle]
  (when-let [b (get object :boxbox)]
    (.applyImpulse b power (+ 90 (to-degrees angle)))))

(defn torque [object power]
  (when-let [b (get object :boxbox)]
    (.applyTorque b power)))

(defn thrust [tx ty]
  (let [player-object (get-in @data [:player :object])
        {px "x" py "y"} (get-position player-object)
        dx (- tx px)
        dy (- ty py)
        a (Math/atan2 dy dx)]
    (swap! data (fn [data]
                  (-> data
                      (assoc-in [:player :thrust] {:dx dx :dy dy :a a :distance (Math/sqrt (+ (* dx dx) (* dy dy)))})
                      (assoc-in [:player :target-angle] a))))))

;; (defn mouse-click-action [event]
;;   (let [player-object (get-in @data [:player :object])
;;         x (.-clientX event)
;;         y (.-clientY event)]
;;     (thrust x y)))

(defn mouse-down-action [event]
  (let [player-object (get-in @data [:player :object])
        x (.-clientX event)
        y (.-clientY event)]
    (thrust x y)))

(defn release-thrust []
  (swap! data (fn [data] (assoc-in data [:player :thrust] nil))))

(defn mouse-up-action [event]
  (release-thrust))

(defn mouse-move-action [event]
  (when (get-in @data [:player :thrust])
    (let [player-object (get-in @data [:player :object])
          x (.-clientX event)
          y (.-clientY event)]
      (thrust x y))))

(em/defaction setup-mouse-events []
  ["canvas"] (em/listen :mousedown mouse-down-action)
  ["canvas"] (em/listen :mouseup mouse-up-action)
  ["canvas"] (em/listen :mousemove mouse-move-action))
;;   ["canvas"] (em/listen :click mouse-click-action))

;; (defn tap-action [event]
;;   (let [x (.-x (aget (.-position event) 0))
;;         y (.-y (aget (.-position event) 0))]
;;     (thrust x y)))

(defn touch-drag-action [event])

(defn setup-touch-events []
  (let [hammer (js/Hammer. (get-canvas)
                           (clj->js {:prevent_default true
                                     :tap true
                                     :drag true
                                     }))]
    ;;(set! (.-ontap hammer) tap-action)
    (set! (.-onstartdrag hammer) touch-dragstart-action)
    ))


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
