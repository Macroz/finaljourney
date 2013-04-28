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
                                             :fill "#eee"
                                             :stroke "#000"
                                             :strokeWidth 2
                                             :rotationDeg a
                                             }))
        shadow (js/Kinetic.Polygon. (clj->js {:x x
                                              :y y
                                              :points points
                                              :fill "#777"
                                              :stroke "#777"
                                              :strokeWidth 2
                                              :rotationDeg a
                                              }))
        brect (.createEntity world (clj->js {:x x
                                             :y y
                                             :points (to-boxbox-points points)
                                             :shape "polygon"
                                             :density 10
                                             :restitution 0.5
                                             :friction 0.01
                                             :maxVelocityX 100000
                                             :maxVelocityY 100000
                                             :color "red"
                                             :rotation a
                                             :draw (fn [])
                                             :onTick (fn []
                                                       (this-as t
                                                                (when t
                                                                  (let [{x "x"
                                                                         y "y"} (js->clj (.position t))
                                                                         a (.rotation t)
                                                                         ox (.getX krect)
                                                                         oy (.getY krect)
                                                                         oa (.getRotationDeg krect)]
                                                                    (.setX shadow ox)
                                                                    (.setY shadow oy)
                                                                    (.setRotationDeg shadow oa)
                                                                    (.setX krect x)
                                                                    (.setY krect y)
                                                                    (.setRotationDeg krect a)
                                                                    ))))
                                             }))
        trail (for [i (range (@data :traillength))]
                (let [c (- 255 (* 255 (/ i (@data :traillength))))
                      c (str "rgb(" c ", " c ", " c ")")]
                  (js/Kinetic.Polygon. (clj->js {:x x
                                                 :y y
                                                 :points [-2 -1 2 0 -2 1]
                                                 :fill c
                                                 :stroke c}))))]
    (.add layer shadow)
    (.add layer krect)
    (doseq [t trail]
      (.add layer t))
    (.moveToBottom shadow)
    (doseq [t trail]
      (.moveToBottom t))
    {:kinetic krect :boxbox brect :points points :shadow shadow :trail trail}))

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
        scale 10000]
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

(defn play-sound [id & options]
  (when-let [sound (get-in @data [:sounds id])]
    (let [{volume :volume} (merge {:volume 1.0}
                                  (apply hash-map options))]
      (set! (.-volume sound) volume)
      (.play sound))))

(defn make-player! []
  (let [layer (get-main-layer)
        {w :screen-width
         h :screen-height} @data
        x (/ w 2)
        y (/ h 2)
        object (make-poly! layer x y 0 [-15 -10 15 0 -15 10])]
    (impulse object 1000000 0)
    (swap! data (fn [data] (assoc data :player {:object object})))
    (.onImpact (get-in @data [:player :object :boxbox])
               (fn [entity normalForce tangentForce]
                 (let [force (* (max (Math/abs normalForce) (Math/abs tangentForce)) 0.0002)]
                   ;;(log "impact " force)
                   (play-sound :hit)
                   (when (> force 10)
                     (swap! data (fn [data]
                                   (update-in data [:player :disabled] + force)))))))))

(defn make-fallen! [size type speed]
  (let [layer (get-main-layer)
        {sw :screen-width
         sh :screen-height} @data
        x (+ sw (rand-int (max 0 (- sw 100))))
        y (rand-int sh)
        w size
        h size
        w2 (/ size 2)
        h2 (/ size 2)
        weight (* w h)
        object (make-poly! layer x y (rand 360) (case type
                                                  0 [(- w2) h2 (- w2) (- h2) w2 h2]
                                                  1 [(- w2) h2 0 (- h2) w2 h2]
                                                  2 [(- w2) h2 w2 (- h2) w2 h2]
                                                  3 [(- w2) (- h2) w2 (- h2) w2 h2 (- w2) h2]
                                                  4 [(- w2) (- h2) (* w2 0.33) (- h2) w2 h2 (* -0.33 w2) h2]
                                                  5 [(- w2) (- h2) (* w2 0.66) (- h2) w2 h2 (* -0.66 w2) h2]
                                                  6 [(- w2) (* h2 0.25) (* w2 -0.33) (- h2) (* w2 0.33) ( - h2) w2 (* h2 0.25) 0 h2]
                                                  7 [(- w2) 0 (/ w -4.5) (- h2) (/ w 4.5) (- h2) w2 0 (/ w 4.5) h2 (/ w -4.5) h2]))]
    (when-let [b (object :boxbox)]
      (.density b weight)
      (.onImpact b
                 (fn [entity normalForce tangentForce]
                   (let [force (* (max (Math/abs normalForce) (Math/abs tangentForce)) 0.0001)]
                     (when (> force 10)
                       (play-sound :hit))))))
    ;;(log "xy " x " " y)
    (impulse object (+ (rand 200000) (rand 300000)) (rand (* 2.0 Math/PI)))
    (impulse object (+ (* speed 1000000)) 3.141)
    (torque object (rand 100000000))
    (swap! data (fn [data] (update-in data [:fallen] conj {:object object})))
    ))

(defn remove-fallen? [fallen]
  (let [{x "x" y "y"} (get-position (fallen :object))]
    (or (< x -50)
        (< y -50)
        (>= y (+ (@data :screen-height) 50)))))

(defn end! []
  (when-not (@data :end?)
    (play-sound :death :volume 0.25)
    (log "end")
    (em/at js/document ["canvas"] (em/chain (em/fade-out 2000)))
    (em/at js/document [".finished"] (em/chain (em/add-class "show black")
                                               (em/fade-in 4000)))
    (em/at js/document [".result"] (em/content "End"))
    (em/at js/document [".score"] (em/content (str (get @data :level))))
    (swap! data (fn [data] (assoc data :end? true)))))

(defn win! []
  (when-not (@data :win?)
    (play-sound :win :volume 0.25)
    (log "win")
    (em/at js/document ["canvas"] (em/chain (em/fade-out 2000)))
    (em/at js/document [".finished"] (em/chain (em/add-class "show white")
                                               (em/fade-in 4000)))
    (em/at js/document [".result"] (em/content "Win"))
    (em/at js/document [".score"] (em/content (str (min 10000 (get @data :level)))))
    (swap! data (fn [data] (assoc data :win? true)))))

(defn signum [x]
  (if (< x 0) -1 1))

(defn update-trail [fallen]
  (when-let [object (fallen :object)]
    (when-let [trail (object :trail)]
      (let [b (object :boxbox)
            {x "x"
             y "y"} (js->clj (.position b))
            a (to-degrees (.rotation b))
            positions (doall (for [t trail]
                               {:x (.getX t)
                                :y (.getY t)
                                :a (.getRotationDeg t)}))
            positions (doall (conj positions {:x x :y y :a a}))]
        (doall (map (fn [t p]
                      (.setX t (p :x))
                      (.setY t (p :y))
                      (.setRotationDeg t (p :a))
                      true)
                    trail
                    positions))))))

(defn tick []
  (when-not (or (@data :end?) (@data :win?))
    (let [level (get @data :level 0)]
      (let [removes (filter remove-fallen? (@data :fallen))
            fallen (remove remove-fallen? (@data :fallen))]
        (swap! data (fn [data]
                      (assoc data :fallen fallen)))
        ;; WTF have to put this here otherwise doseq is skipped
        (count fallen)
        (doseq [f removes]
          ;;(log "removing " f)
          (let [{boxbox :boxbox
                 kinetic :kinetic
                 shadow :shadow
                 trail :trail} (f :object)]
            (when boxbox
              (.destroy boxbox))
            (when kinetic
              (.destroy kinetic))
            (when shadow
              (.destroy shadow))
            (when trail
              (doseq [t trail]
                (.destroy t))))))
      (when (= 0 (mod level 5))
        (doseq [f (@data :fallen)]
          (update-trail f))
        (update-trail (get @data :player)))
      (let [player-object (get-in @data [:player :object])
            disabled (get-in @data [:player :disabled] 0)
            a (get-heading player-object)]
        (when-not (get-in @data [:player :born?] false)
          (play-sound :birth :volume 0.25)
          (swap! data (fn [data] (assoc-in data [:player :born?] true))))
        (if (> disabled 0)
          (do
            (.setFill (player-object :kinetic) "#aaa")
            (swap! data (fn [data]
                          (assoc-in data [:player :disabled] (max 0 (dec disabled)))))
            (when (<= (get-in @data [:player :disabled] 0) 0)
              (play-sound :repaired :volume 0.5)))
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
                (let [distance (min 400 (thrust :distance 0))
                      volume (min 0.4 (/ distance 400))]
                  (play-sound :thrust :volume volume)
                  (impulse player-object (* distance 1000) (* Math/PI (/ a 180.0))))))
            ;;(log "a " a " ta " ta " da " da)
            (torque player-object (* (+ p i d) 1000000))
            (swap! data (fn [data] (-> data
                                       (update-in [:player :target-error] + da)
                                       (assoc-in [:player :target-error-last] da))))))
        (let [{px "x" py "y"} (get-position player-object)]
          (let [death (get @data :death)
                scale (max 0 (/ (- 400 px) 400))]
            (.setOpacity death scale scale)
            (.setX death (min 0 (- (* px 2))))
            (.setScale death (+ 0.2 (* scale 0.8)))
            (.moveToTop death)
            (when (or (< px 0)
                      (< py 0)
                      (>= py (@data :screen-height)))
              (end!)))
          (let [life (get @data :life)
                scale (max 0 (/ (- level 7000) 3000))]
            (.setOpacity life scale)
            (.setX life (- (+ (@data :screen-width) 100) (max 0 (/ (- level 7000) 20))))
            (.setScale life (+ 0.2 (* scale 1.9)))
            (.moveToTop life)
            (when (and (>= level 10000) (> px (* (@data :screen-width) 0.8)))
              (win!))
            )))
      (let [speed 1
            fallen (get @data :fallen)
            target-fallen (cond (< level 300) 1
                                (< level 500) 3
                                (< level 1000) 5
                                (< level 1500) 10
                                (< level 2000) 20
                                (< level 2300) 1
                                (< level 2500) 5
                                (< level 3500) 10
                                (< level 4000) 20
                                (< level 5000) 10
                                (< level 7000) 20
                                :else 8)]
        (if (< (count fallen) target-fallen)
          (let [size-min (/ level 500)
                size (cond (< level 2500) 15
                           (< level 3000) 25
                           (< level 4000) 50
                           (< level 5000) 30
                           (< level 6000) 80
                           (< level 8000) 40
                           (< level 9000) 100
                           (< level 10000) 130
                           :else 50)
                size (+ size-min (rand size) (rand size) (rand size))
                type (cond (< level 1000) 0
                           (< level 2500) (rand-int 3)
                           (< level 4000) (+ 3 (rand-int 3))
                           (< level 5000) 6
                           (< level 7000) (rand-int 7)
                           :else 7)
                speed (cond (< level 4000) (/ level 2500)
                            (< level 5000) (+ 2 (rand 5) (rand 5) (rand 5))
                            (< level 7000) (+ 2 (rand 9) (rand 9) (rand 9))
                            :else 1)]
            (make-fallen! size type speed)))
        (swap! data (fn [data]
                      (update-in data [:level] (fn [x] (min 10000 (+ x speed))))))
        (let [gravity (clj->js {:x (min -1 (- (/ (- 6000 level) 10))) :y 0})]
          (.gravity (@data :world) gravity))
        (when (= 0 (mod level 100))
          (em/at js/document ["body"] (em/set-attr :style (str "background-color: " (get-level-color)))))
        ))))


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
      (.onRender world (fn [ctx]
                         (.draw stage)
                         (let [level (get @data :level 0)]
                           (if (< level 5000)
                             (set! (.-fillStyle ctx) "#fff")
                             (set! (.-fillStyle ctx) "#000"))
                           (.fillText ctx (str (get @data :level 0)) 10 20))
                         ))
      (.onTick world tick))
    (make-player!)
    (let [sw (@data :screen-width)
          sh (@data :screen-height)
          sh2 (/ sh 2)
          life (js/Kinetic.Group. (clj->js {:x sw
                                            :y sh2
                                            :opacity 0
                                            }))
          lifes (map (fn [[x y r]]
                       (js/Kinetic.Circle. (clj->js {:x x
                                                     :y y
                                                     :radius r
                                                     :fill "#fff"
                                                     :stroke "#fff"
                                                     :strokeWidth 2})))
                     [[(/ sw -9) (/ sh2 -1.0) (/ sh 2.8)]
                      [(/ sw -6) (/ sh2 -3.0) (/ sh 4)]
                      [(/ sw -10) (/ sh2 1.0) (/ sh 2.9)]
                      [(/ sw -12) (/ sh2 7) (/ sh 3.2)]
                      [(/ sw 5) 0 sh]
                      ])
          death (js/Kinetic.Polygon. (clj->js {:x 0
                                               :y sh2
                                               :points [-5 (- sh2)
                                                        (/ sw 2.00) (- 0 sh2)
                                                        (/ sw 1.50) (- (/ sh 8.0) sh2)
                                                        (/ sw 1.30) (- (/ sh 3.5) sh2)
                                                        (/ sw 1.70) (- (/ sh 6.0) sh2)
                                                        (/ sw 5.00) (- (/ sh 9.0) sh2)
                                                        (/ sw 2.30) (- (/ sh 4.0) sh2)
                                                        (/ sw 1.60) (- (/ sh 3.5) sh2)
                                                        (/ sw 2.10) (- (/ sh 3.1) sh2)
                                                        (/ sw 4.50) (- (/ sh 2.8) sh2)
                                                        (/ sw 6.00) (- (/ sh 2.5) sh2)
                                                        (/ sw 5.00) (- (/ sh 2.2) sh2)
                                                        (/ sw 2.50) (- (/ sh 2.1) sh2)
                                                        (/ sw 1.80) (- (/ sh 2.9) sh2)
                                                        (/ sw 2.10) (- (/ sh 2.0) sh2)
                                                        (/ sw 3.10) (- (/ sh 1.7) sh2)
                                                        (/ sw 4.00) (- (/ sh 1.3) sh2)
                                                        (/ sw 2.10) (- (/ sh 1.2) sh2)
                                                        (/ sw 1.80) (- (/ sh 1.3) sh2)
                                                        (/ sw 2.10) (- (/ sh 1.1) sh2)
                                                        (/ sw 1.35) (- (/ sh 1.2) sh2)
                                                        (/ sw 1.15) (- (/ sh 1.5) sh2)
                                                        (/ sw 1.30) (- (/ sh 1.1) sh2)
                                                        (/ sw 1.40) (- (/ sh 1.05) sh2)
                                                        (/ sw 1.60) (- sh sh2)
                                                        -5 (- sh sh2)]
                                               :fill "#000"
                                               :stroke "#333"
                                               :strokeWidth 2
                                               :opacity 0
                                               }))]
      (.add layer death)
      (doseq [l lifes]
        (.add life l))
      (.add layer life)
      (swap! data (fn [data]
                    (-> data
                        (assoc :fallen [])
                        (assoc :death death)
                        (assoc :life life)))))))

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

(defn touch-hold-action [event]
  (let [x (.-x (aget (.-position event) 0))
        y (.-y (aget (.-position event) 0))]
    (thrust x y)))

(defn touch-dragend-action [event]
  (release-thrust))

(defn touch-release-action [event]
  (release-thrust))

(defn touch-drag-action [event]
  (let [x (.-x (.-position event))
        y (.-y (.-position event))]
    (thrust x y)))

(defn setup-touch-events []
  (let [hammer (js/Hammer. (get-canvas)
                           (clj->js {:prevent_default true
                                     :drag true
                                     :touch true
                                     :release true
                                     :hold_timeout 10
                                     }))]
    (set! (.-onhold hammer) touch-hold-action)
    (set! (.-ondragend hammer) touch-dragend-action)
    (set! (.-onrelease hammer) touch-release-action)
    (set! (.-ondrag hammer) touch-drag-action)
    ))


(defn setup-controls []
  (setup-mouse-events)
  (setup-touch-events))

(defn setup-sounds []
  (swap! data (fn [data]
                (-> data
                    (assoc-in [:sounds :thrust] (js/Audio. "/snd/thrust.wav"))
                    (assoc-in [:sounds :death] (js/Audio. "/snd/longbeep.wav"))
                    (assoc-in [:sounds :birth] (js/Audio. "/snd/birth.wav"))
                    (assoc-in [:sounds :hit] (js/Audio. "/snd/hit.wav"))
                    (assoc-in [:sounds :win] (js/Audio. "/snd/win.wav"))
                    (assoc-in [:sounds :repaired] (js/Audio. "/snd/powerup.wav"))))))

(defn startup [params]
  (swap! data (fn [data] (merge data {:traillength 3} (js->clj params :keywordize-keys true))))
  (log "startup")
  (setup-screen)
  (setup-world)
  (setup-controls)
  (setup-sounds)
  (set! (.-onresize js/window) resize)
  (em/at js/document ["canvas"] (em/chain (em/fade-in 2000)))
  (log "startup done"))
