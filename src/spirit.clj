(ns spirit
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.java.shell :as sh]
            [hato.client :as http]
            [instaparse.core :as insta]            

            [ring.adapter.jetty :as rj]
            [ring.middleware.defaults :as rd]
            [ring.util.response :as rr]
            [ring.util.codec]
            [compojure.core :refer [GET POST defroutes]]

            [spirit.weather :refer [query-weather]]
            [spirit.whisper :refer [run-whisper]]
            [spirit.lms :as lms]
            [spirit.grammar :as grammar]
            [spirit.timer :as timer])
  (:import [java.time LocalTime]
           [java.security MessageDigest])
  (:gen-class))

(def ^:dynamic *config* {:ha/url ""
                         :ha/token ""
                         :ha/args {}
                         :lms/player-name ""
                         :lms/server ""
                         :web/port 12346
                         :web/address "http://slab.home:12346/"
                         })

(defn sound-url [sound]
  (str (:web/address *config*)
       "/sound/" (name sound)))

(defn tts-url [message]
  (str (:web/address *config*) "/speak?message="
       (ring.util.codec/url-encode message)))

(defn play-sounds [& sounds]
  (try (lms/play-urls
        (:lms/server *config*)
        (:lms/player-name *config*)
        (doall
         (for [sound sounds]
           (if (keyword? sound)
             [(sound-url sound) (name sound)]
             [(tts-url sound) sound]))))
       (catch Exception e
         (log/error e "Playing sounds" *config*)
         )))

(defn ha-call [url body]
  (let [resp (http/post
              (str (:ha/url *config*) url)

              {:accept :json
               :as :json
               :content-type :json
               :form-params body
               :headers {"Authorization" (format "Bearer %s"
                                                 (:ha/token *config*))}})]
    (log/info "CALL" url body "=>" (:status resp) (:body resp))
    resp))

(defn speak [message]
  (play-sounds message))


(defmulti handle-command
  (fn [c] (keyword (or (namespace (:command c))
                       ::no-namespace))))

(defmethod handle-command :ha [{:keys [command args] :as c}]
  (let [service (name command)
        beep (future
               (when-not (and
                          (not (:spirit/say args))
                          (or (lms/is-playing? (:lms/server *config*)
                                               (:lms/player-name *config*))
                              (:spirit/quiet args)))
                 (let [msg (:spirit/say args :success)]
                   (play-sounds
                    (if (string? msg)
                      (reduce-kv
                       (fn [msg k v] (string/replace msg (str k) (str v)))
                       msg
                       args)
                      msg)))))
        ]
    (let [status
          (future
            (:status
             (ha-call (str "/services/" (string/replace service #"\." "/"))
                      (merge (:ha/args *config*) (dissoc args
                                                         :spirit/quiet
                                                         :spirit/say)))))

          status @status
          _ @beep]
      (when-not (= 200 status)
        (play-sounds :command-error "Error from home assistant")))))


(defmethod handle-command :timer [{:keys [command args]}]
  (let [bindings (get-thread-bindings)
        play-sounds- (fn [& sounds] (with-bindings bindings (apply play-sounds sounds)))]
    (timer/handle-timer command args play-sounds-)))

(defmethod handle-command :query [{c :command args :args}]
  (case c
    :query/weather
    (speak (query-weather (:day args)))
    
    :query/time
    (speak
     (let [t (LocalTime/now)
           h (.getHour t)
           m (.getMinute t)]
       (str "the time is "
            (grammar/digit-values (mod h 12)) " "
            (when (< m 10) "oh ")
            (if-let [m (grammar/digit-values m)]
              m
              (str (grammar/digit-values (* 10 (int (/ m 10))))
                   " "
                   (grammar/digit-values (mod m 10)))
              )))
     )))

(defmethod handle-command :default [c]
  (log/error "Unknown command" c))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn random-sound [name n]
  (-> (rr/resource-response
       (str "/sounds/" name (inc (rand-int n)) ".mp3"))
      (rr/content-type "audio/mp3")))

(defn- handle-message [message player-name params]
  (binding [*config*
            (-> *config*
                (assoc  :lms/player-name player-name)
                (update :ha/args merge params))]
    
    (let [result ((::grammar *config*) message)]
      (if (insta/failure? result)
        (do (log/error
             "Unable to parse command"
             message result)
            (speak (str "I don't understand " message))
            (-> (rr/response "Command not parseable")
                (rr/status 500)))
        
        (do (log/info "Parsed" message "to" result "reply via" player-name params)
            (try (handle-command result)
                 (rr/response "ok")
                 (catch Exception e
                   (play-sounds :exception)
                   (log/error e "in handling" result)
                   (-> (rr/response "error in handler")
                       (rr/status 500)))))))))

(defroutes spirit-routes
  (GET "/speak" [message]
    (let [m (md5 message)
          f (io/as-file (str "/tmp/" m ".wav"))]
      (when-not (.exists f)
        (sh/sh "mimic" "-t" message (str m ".wav") :dir "/tmp"))
      (-> (rr/file-response (.getCanonicalPath f))
          (rr/content-type "audio/wav")
          (rr/header "Content-Disposition" "inline; filename=message.wav"))))

  (GET "/sound/:s" [s]
    ;; resource responses
    (case (keyword s)
      :success (random-sound "success" 4)
      :chime   (random-sound "chime" 2)
      (random-sound "error" 3)))

  (POST "/hear" [player-name & params :as request]
    (let [temp-file (.toFile (java.nio.file.Files/createTempFile
                              "hear" ".wav"
                              (into-array java.nio.file.attribute.FileAttribute [])))]
      (try
        (io/copy (:body request) temp-file)
        (log/info "running whisper on" temp-file)
        (let [message (run-whisper temp-file)
              command (grammar/detect-wakeword message)]
          (log/info message "=>" command)
          (if command
            (handle-message command player-name params)
            (rr/response (str "no wakeword in " message "\n"))))
        (finally
          (io/delete-file temp-file true)))))
  
  (POST "/read" [message player-name & params]
    (log/info "hearing" message)
    (handle-message message player-name params)))

(defn handler [bindings request]
  (with-bindings bindings
    (spirit-routes request)))

(defn run-http-server [port]
  (let [bindings (get-thread-bindings)]
    (rj/run-jetty
     (rd/wrap-defaults
      ;; not sure whether I need to convey bindings like this
      (partial #'handler bindings)
      (assoc-in rd/site-defaults [:security :anti-forgery] false))
     {:port port :join? true})))

(defn run [{:keys [grammar config]}]
  (binding [*config*
            (let [grammar (grammar/load-grammar grammar)]
              (-> (edn/read (java.io.PushbackReader. (io/reader (io/as-file config))))
                  (assoc ::grammar grammar)))]
    (run-http-server (:web/port *config*))))

(defn -main [grammar config]
  (run {:grammar grammar :config config}))

