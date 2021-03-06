(ns finaljourney.server
  (:use compojure.core)
  (:use clojure.test)
  (:use [clojure.walk :only [keywordize-keys]])
  (:use [ring.middleware.params :only [wrap-params]])
  (:require [compojure.route :as route])
  (:use [ring.adapter.jetty :only [run-jetty]])
  (:use [hiccup core element page]))

(defn link-image [src]
  [:a {:href src}
   [:img {:src src
          :width 300
          :height 200}]])

(defn intro-page []
  (html5 [:html
          [:head
           [:title "Final Journey"]
           (include-css "css/reset.css")
           (include-css "css/main.css")]
          [:body
           [:div.center
            [:div.content
             [:div.space]
             [:h1 "Final Journey"]
             [:div.space]
             [:a.begin {:href "/game"} [:h1 "Begin"]]
             [:div.space]
             [:div.info
              [:p [:a {:href "http://ludumdare.com/compo/"} "Ludum Dare #26 Game Programming Competition"] " entry by " [:a {:href "http://markku.rontu.net"} "Markku Rontu / markku.rontu@iki.fi / @zorcam"]]
              [:p "Touch or mouse to play. Google Chrome recommended for speed."]
              [:p "Finished, awesome! Only problem this time was JavaScript performance. Had tons of fun! #win"]
              [:p [:a {:href "http://www.github.com/Macroz/finaljourney/"} "Source and more info"]]]
             ]]]]))

(defn clj->js [x]
  (str "{" (apply str (interpose "," (map (fn [e]
                                            (str (if (keyword? (key e))
                                                   (name (key e))
                                                   (key e)) ": "
                                                   (if (re-matches #"\d+" (val e))
                                                     (val e)
                                                     (str "'" (val e) "'")))) x))) "}"))

(defn game-page [params]
  (html5 [:html
          [:head
           [:title "Final Journey"]
           [:meta {:name "viewport"
                   :content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"}]
           [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
           [:meta {:name "apple-mobile-web-app-status-bar-style" :content "black-translucent"}]
           (include-css "css/reset.css")
           (include-css "css/main.css")
           (include-js "js/kinetic.js")
           (include-js "js/Box2dWeb-2.1.a.3.min.js")
           (include-js "js/boxbox.js")
           (include-js "js/hammer.js")
           (include-js "js/cljs.js")]
          [:body {:onload (str "finaljourney.main.startup(" (clj->js params) ");")}
           [:div.finished.center
            [:div.content
             [:div.space]
             [:h1 "Final Journey"]
             [:div.space]
             [:h1.result "End"]
             [:div.space]
             [:div.score]
             ]]
           [:div#container]]]))

(defroutes handler
  (GET "/" {params :params} (intro-page))
  (GET "/game" {params :params} (game-page (keywordize-keys params)))
  (route/resources "/")
  (route/not-found "Page not found!"))

(def app (-> handler
             wrap-params
             ))

(def server (atom nil))

(defn- run []
  (when @server
    (.stop @server))
  (swap! server (fn [old]
                  (run-jetty #'app {:port 8080 :join? false}))))

(defn -main [port]
  (run-jetty #'app {:port (Integer. port)}))
