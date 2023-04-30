(ns spirit.timer
  (:require [chime.core :as chime]
            [clojure.string :as string]
            [clojure.tools.logging :as log])
  (:import [java.time Instant]))

(def timers (atom {}))

(defn time-difference [now then]
  (let [delta (abs (.getSeconds (java.time.Duration/between now then)))
        minutes (int (/ delta 60))
        seconds (mod delta 60)
        hours (int (/ minutes 60))
        minutes (mod minutes 60)
        ]
    (cond
      (= 0 seconds minutes hours) "now"

      :else
      (cond-> ""
        (= 1 hours)   (str "1 hour ")
        (> hours 1)   (str hours " hours ")
        (= 1 minutes) (str "1 minute ")
        (> minutes 1) (str minutes " minutes ")
        (= 1 seconds) (str "1 second")
        (> seconds 1) (str seconds " seconds")))))

(defn handle-timer [command args play-sounds]
  (let [{:keys [time n]} args]
    (case command
      :timer/set
      (let [now (Instant/now)
            then (.plusSeconds now time)
            timer-name (atom nil)]
        (swap!
         timers
         (fn set-timer [cur-timers]
           (let [next-id (first (remove (set (keys cur-timers)) (rest (range))))]
             (reset! timer-name next-id)
             (assoc cur-timers next-id
                    [then (chime/chime-at
                           [then]
                           (fn [_]
                             (log/info "timer" next-id "finished")
                             (swap! timers dissoc next-id)
                             (play-sounds :chime (format "Timer %s finished" next-id)))
                           )]))))
        (play-sounds (format "Timer %s started for %s" @timer-name (time-difference now then))))
      
      :timer/cancel (do (.close (get @timers n))
                        (swap! timers dissoc n)
                        (play-sounds (format "Timer %s cancelled" n)))
      :timer/list   (let [timers @timers
                          now (Instant/now)
                          message (string/join
                                   ", "
                                   (for [[timer-id [time _]] timers]
                                     (format "timer %s in %s"
                                             timer-id
                                             (time-difference now time))))]
                      (play-sounds message)))))
