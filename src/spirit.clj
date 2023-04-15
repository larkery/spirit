(ns spirit
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.string :as string]
            [clj-fuzzy.metrics :refer [levenshtein]]
            [hato.client :as http]
            [chime.core :as chime])
  (:import [java.time Instant])
  (:gen-class))

(def metagrammar (insta/parser (io/resource "metagrammar.g")))
(def builtins    (insta/parser (io/resource "builtins.g")))
(def word-number-re
  (let [digit "(one|two|three|four|five|six|seven|eight|nine)"
        ten   "(ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen)"
        tens  "(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)"
        small (format "((?<tens>%s)( +(?<tendigit>%s))?|(?<digit>%s)|(?<ten>%s))" tens digit digit ten)
        ]
    (re-pattern small)))

(def digit-values {"one"     1  "two"       2  "three"    3  "four"     4  "five"    5
                   "six"     6  "seven"     7  "eight"    8  "nine"     9  "ten"     10
                   "eleven"  11 "twelve"    12 "thirteen" 13 "fourteen" 14 "fifteen" 15
                   "sixteen" 16 "seventeen" 17 "eighteen" 18 "nineteen" 19 "twenty"  20
                   "thirty"  30 "forty"     40 "fifty"    50 "sixty"    60 "seventy" 70
                   "eighty"  80 "ninety"    90})

(defn replace-word-numbers [s]
  (let [matcher (re-matcher word-number-re s)
        sb (StringBuffer.)]
    (loop []
      (if (.find matcher)
        (let [tens     (.group matcher "tens")
              tendigit (.group matcher "tendigit")
              digit    (.group matcher "digit")
              ten      (.group matcher "ten")

              rep (cond
                    tens  (+ (digit-values tens) (if tendigit (digit-values tendigit) 0))
                    digit (digit-values digit)
                    ten   (digit-values ten))
              
              ]
          (.appendReplacement matcher sb (str rep))
          (recur))
        (do (.appendTail matcher sb)
            (.toString sb))))))

(defn normalize [s]
  (-> s
      (string/replace #"[,.?! ]+" " ")
      (replace-word-numbers)
      (string/replace #"%" " percent")
      (string/lower-case)))

(defn parse-with-fuzz [g s]
  (let [s (normalize s)
        r (g s)]
    (if (insta/failure? r)
      (let [{:keys [index reason]} (insta/get-failure r)
            up-to  (subs s 0 index)
            rest-s (subs s index)
            [w rest-s] (string/split rest-s #" " 2)
            options
            (->> reason
                 (filter (comp #{:string} :tag))
                 (map :expecting)
                 (map (fn find-l [e] [(levenshtein w e) e]))
                 (filter (fn filter-l [%] (< (first %) (/ (count (second %)) 2))))
                 (sort))
            ]
        (or (first (keep
                    (fn keep-parses [[ld e]]
                      (let [s (str up-to " " e " " rest-s)]
                        (let [r (parse-with-fuzz g s)]
                          (when-not (insta/failure? r)
                            r))))
                    options))
            r))
      r)))

(defn load-grammar [filename]
  (let [g (metagrammar (slurp (io/as-file filename)))
        _ (when (insta/failure? g) (log/error "Parsing" filename g))
        wrap-strings #(map (fn wrap-strings [i]
                             (if (string? i)
                               (c/cat (c/hide (c/regexp #"\s*"))
                                      (c/string i)
                                      (c/hide (c/regexp #"\s*")))
                               
                               i))
                           %)
        
        extra (atom {})
        ctr (atom 0)
        gr (insta/transform
            {:G (fn transform-g [& items] (vec items))
             :command (fn command
                        ([label args sentence]
                         (let [label0 (keyword label)
                               label (keyword (str label "_" (swap! ctr inc)))]
                           (swap! extra
                                  #(-> (update % :commands assoc label label0)
                                       (update :command-args assoc label args)))
                           [label sentence]))
                        ([label sentence]
                         (command label nil sentence)))
             
             :grammar (fn grammar [label sentence] [(keyword label) sentence])
             :grammar-rule (fn grule [label] (c/nt (keyword label)))
             :sentence c/cat
             :word-sequence (fn word-seq [& items]
                              (apply c/cat (wrap-strings items)))
             :optional-words (fn opt-words [& items]
                               (c/opt (apply c/cat (wrap-strings items))))
             :capture (fn capture [label sentence]
                        (let [label0 (keyword label)
                              label (keyword (str label "_" (swap! ctr inc)))]
                          (swap! extra update :captures conj [label label0 sentence])
                          (c/nt label)))
             :parameters read-string
             :alternates c/alt}
            g
            )
        gr (into {} gr)
        {:keys [captures commands command-args]} @extra
        gr (reduce (fn add-captures [a [l l0 r]] (assoc a l r)) gr captures)

        parser
        (insta/parser (merge
                       (:grammar builtins)
                       (assoc gr :root (apply c/alt (map c/nt (keys commands)))))
                      :start :root)
        ]
    
    (fn parse-line [line]
      (let [result (insta/transform
                    (merge
                     (into {} (for [[l l0 _] captures] [l (fn captures [v] [l0 v])]))
                     {:integer read-string
                      :hours   (fn [h & [m]] (+ (or m 0) 60 (* 60 h)))
                      :minutes (fn [m] (* 60 m))
                      :seconds identity
                      :time    identity
                      :root    (fn root [hs]
                                 {:command (first hs)
                                  :args    (into {} (remove string? (rest hs)))})})

                    (parse-with-fuzz parser line))]

        (-> (update result :args (fn merge-args [%]
                                   (merge (command-args (:command result)) %)))
            (assoc :command (commands (:command result))))
        ))))

(def ^:dynamic *config* {:ha/url ""
                         :ha/token ""
                         :ha/args {}})
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

(defn lms [player command & args]
  (loop [n 0]
    (let [resp
          (-> (ha-call "/services/squeezebox/call_query"
                       (cond-> {:entity_id player :command (name command)}
                         (seq args) (assoc :parameters (vec (map name args)))))
              (:body))]
      (if (and (= [] resp) (< n 5))
        (do (Thread/sleep 750) (recur (inc n)))
        (-> resp
            (get 0)
            (:attributes)
            (:query_result))))))

(defn play-urls [player urls]
  (let [mode (-> (lms player :mode :?) :_mode)
        time (-> (lms player :time :?) :_time)]
    (let [[[url title] & urls] urls]
      (lms player :playlist :preview
           (str "url:" url) (str "title:" title))
      (doseq [[url title] urls]
        (lms player :playlist :add
             (str "url:" url) (str "title:" title))))
    
    (loop []
      (Thread/sleep 1500)
      (when (= "play" (:_mode (lms player :mode :?))) (recur)))
    
    (lms player :playlist :preview "cmd:stop")
    (when (= mode "play")
      (lms player :play "1")
      (Thread/sleep 1000)
      (log/info "seek back to" time)
      (lms player :time (str time))
      (lms player :time (str time)))))

(defn sound-url [sound]
  (case sound
    :command-error (str (:sound-prefix *config*) "/error.mp3")
    :success (str (:sound-prefix *config*) "/"
                  (first (shuffle ["chirp1" "chirp2"])) ".mp3")
    :chime (str (:sound-prefix *config*) "/chirp1.mp3")))

(defn tts-url [message]
  (:url (:body (ha-call
                "/tts_get_url"
                {:platform "google_translate"
                 :message message}))))

(defn speak [message]
  (play-urls
   (:ha/media-player *config*)
   [[(tts-url message) message]]))

(defmulti handle-command
  (fn [c] (keyword (or (namespace (:command c))
                       ::no-namespace))))

(defmethod handle-command :ha [{:keys [command args]}]
  (let [service (name command)]
    (let [status
          (:status
           (ha-call (str "/services/" (string/replace service #"\." "/"))
                    (merge (:ha/args *config*) args)))
          player (:ha/media-player *config*)]
      (if (= 200 status)
        (when-not (= "play" (:_mode (lms player :mode :?)))
          (play-urls player [[(sound-url :success) "Success"]]))
        (play-urls player [[(sound-url :command-error) "Command error"]])))))

(def timers (atom {}))


(defn time-difference [now then]
  (let [delta (abs (.getSeconds (java.time.Duration/between now then)))
        minutes (int (/ delta 60))
        seconds (mod delta 60)
        hours (int (/ minutes 60))
        minutes (mod minutes 60)
        ]
    (cond
      (and (= 0 seconds minutes hours)) "now"

      :else
      (cond-> ""
        (= 1 hours)   (str "1 hour ")
        (> hours 1)   (str hours " hours ")
        (= 1 minutes) (str "1 minute ")
        (> minutes 1) (str minutes " minutes ")
        (= 1 seconds) (str "1 second")
        (> seconds 1) (str seconds " seconds")))))


(defmethod handle-command :timer [{:keys [command args]}]
  (let [{:keys [time n]} args]
    (case command
      :timer/set    (let [now (Instant/now)
                          then (.plusSeconds now time)
                          timer-name (atom nil)
                          config *config*]
                      (swap! timers
                             (fn set-timer [cur-timers]
                               (let [next-id (first (remove (set (keys cur-timers)) (rest (range))))]
                                 (reset! timer-name next-id)
                                 (assoc cur-timers next-id
                                        [then (chime/chime-at
                                               [then]
                                               (fn [_]
                                                 (log/info "timer" next-id "finished")
                                                 (swap! timers dissoc next-id)
                                                 (binding [*config* config]
                                                   (play-urls
                                                    (:ha/media-player config)
                                                    [[(tts-url (format "Timer %s finished" next-id))
                                                      "Timer finished"]
                                                     [(sound-url :chime) "Chime"]]))))]))))
                      (speak (format "Timer %s started for %s" @timer-name (time-difference now then))))
      
      :timer/cancel (do (.close (get @timers n))
                        (swap! timers dissoc n)
                        (speak (format "Timer %s cancelled" n)))
      :timer/list   (let [timers @timers
                          now (Instant/now)
                          message (string/join
                                   ", "
                                   (for [[timer-id [time _]] timers]
                                     (format "timer %s in %s"
                                             timer-id
                                             (time-difference now time))))]
                      (speak message)))))

(defmethod handle-command :default [c]
  (log/error "Unknown command" c))

(defn run [{:keys [grammar config]}]
  (binding [*config* (edn/read (java.io.PushbackReader.
                                (io/reader (io/as-file config))))]
    (let [parser (load-grammar grammar)]
      (loop []
        (let [line (read-line)]
          (when (string/starts-with? line "COM ")
            (let [line (subs line 4)
                  result (parser line)]
              (if (insta/failure? result)
                (do (log/error
                     "Unable to parse command"
                     line result)
                    (speak (str "I don't understand " line)))
                (do (log/info "Parsed" line "to" result)
                    (handle-command result)))))
          (when-not (= line "quit") (recur)))))))

(defn -main [grammar config]
  (run {:grammar grammar :config config}))
