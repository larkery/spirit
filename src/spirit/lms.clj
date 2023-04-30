(ns spirit.lms
  (:require [hato.client :as http]
            [clojure.tools.logging :as log]))

(def player-by-name
  (memoize
   (fn [server-name player-name]
     (-> (http/post
          server-name
          {:accept :json
           :as :json
           :content-type :json
           :form-params
           {:id 0
            :method "slim.request"
            :params ["" ["serverstatus" 0 100]]}})
         (:body)
         (:result)
         (:players_loop)
         (->> (map (juxt :name identity))
              (into {}))
         (get player-name)))))

(defn command [server-name player-name & command]
  (if-let [player (player-by-name server-name player-name)]
    (-> (http/post
         server-name
         {:accept :json
          :as :json
          :content-type :json
          :form-params
          {:method "slim.request"
           :params [(:playerid player) (mapv name command)]}})
        (:body)
        (:result))
    (log/warn "No player found by name" player-name "at" server-name)))

(defn play-urls [server-name player-name urls]
  (let [lms    (fn [& r] (apply command server-name player-name r))
        model  (:modelname (player-by-name server-name player-name))
        mode   (-> (command server-name player-name :mode :?) :_mode)
        time   (-> (command server-name player-name :time :?) :_time)
        volume (-> (command server-name player-name :mixer :volume :?) :_volume)]

        (lms :power "1")
    (lms :mixer :volume "75")
    (let [[[url title] & urls] urls]
      (lms :playlist :preview
           (str "url:" url) (str "title:" title))
      
      (doseq [[url title] urls]
        (if title
          (lms :playlist :add url title)
          (lms :playlist :add url))))
    (lms :play)
    (let [sleep-time (case model
                       "RaopBridge" 500
                       200)]
      (Thread/sleep sleep-time)
      (loop []
        (when (= "play" (:_mode (lms :mode :?)))
          (Thread/sleep sleep-time)
          (recur)))

      (lms :playlist :preview "cmd:stop")
      (lms :mixer :volume volume)
      (when (= mode "play")
        (lms :play "1")
        (Thread/sleep sleep-time)
        (log/info "seek back to" time)
        (lms :time (str time))
        (lms :time (str time))))))

(defn is-playing? [server-name player-name]
  (= "play" (:_mode (command server-name player-name :mode :?))))

