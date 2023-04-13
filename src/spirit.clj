(ns spirit
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.string :as string]
            [clj-fuzzy.metrics :refer [levenshtein]]
            [hato.client :as http])
  (:gen-class))

(def metagrammar (insta/parser (io/resource "metagrammar.g")))
(def builtins    (insta/parser (io/resource "builtins.g")))

(defn normalize [s]
  (-> s
      (string/replace #"[. ,-?!]+" " ")
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
                 (map (fn [e] [(levenshtein w e) e]))
                 (filter #(< (first %) (/ (count (second %)) 2)))
                 (sort))
            ]
        (or (first (keep
                    (fn [[ld e]]
                      (let [s (str up-to " " e " " rest-s)]
                        (let [r (parse-with-fuzz g s)]
                          (when-not (insta/failure? r)
                            r))))
                    options))
            r))
      r)))

(defn load-grammar [filename]
  (let [g (metagrammar (slurp (io/as-file filename)))
        _ (when (insta/failure? g)
            (println g))
        wrap-strings #(map (fn [i]
                             (if (string? i)
                               (c/cat (c/hide (c/regexp #"\s*"))
                                      (c/string i)
                                      (c/hide (c/regexp #"\s*")))
                               
                               i))
                           %)
        
        extra (atom {})
        ctr (atom 0)
        gr (insta/transform
            {:G (fn [& items] (vec items))
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
        gr (reduce (fn [a [l l0 r]] (assoc a l r)) gr captures)

        parser
        (insta/parser (merge
                       (:grammar builtins)
                       (assoc gr :root (apply c/alt (map c/nt (keys commands)))))
                      :start :root)
        ]
    
    (fn parse-line [line]
      (let [result (insta/transform
                    (merge
                     (into {} (for [[l l0 _] captures] [l (fn [v] [l0 v])]))
                     {:integer read-string
                      :hours (fn [h & [m]] (+ (or m 0) 60 (* 60 h)))
                      :minutes (fn [m] (* 60 m))
                      :seconds identity
                      :time identity
                      :root (fn [hs]
                              {:command (first hs)
                               :args (into {} (remove string? (rest hs)))})})

                    (parse-with-fuzz parser line))]

        (-> (update result :args #(merge (command-args (:command result)) %))
            (assoc :command (commands (:command result))))
        ))))

(def ^:dynamic *config* {:ha/url ""
                         :ha/token ""
                         :ha/args {}})
(defn ha-call [url body]
  (-> (http/post
       (str (:ha/url *config*) url)

       {:accept :json
        :as :json
        :content-type :json
        :form-params body
        :headers {"Authorization" (format "Bearer %s"
                                          (:ha/token *config*))}})
      
      (:body)))

(defn lms [player command & args]
  (loop [n 0]
    (let [resp
          (-> (ha-call "/services/squeezebox/call_query"
                       (cond-> {:entity_id player :command (name command)}
                         (seq args) (assoc :parameters (vec (map name args))))))]
      (if (and (= [] resp) (< n 5))
        (do (Thread/sleep 500) (recur (inc n)))
        (-> resp
            (get 0)
            (:attributes)
            (:query_result))))))

(defn play-urls [player urls]
  (let [mode (-> (lms player :mode :?) :_mode)
        time (-> (lms player :time :?) :_time)]
    (let [[[url title] & urls] urls]
      (lms player :playlist :preview (str "url:" url) (str "title:" title))
      (doseq [[url title] urls]
        (lms player :playlist :add (str "url:" url) (str "title:" title))))
    
    (loop []
      (Thread/sleep 1500)
      (when (= "play" (:_mode (lms player :mode :?))) (recur)))
    
    (lms player :playlist :preview "cmd:stop")
    (when (= mode "play")
      (lms player :play "1")
      (Thread/sleep 750)
      (lms player :time (str time)))))

(def ^:dynamic *media-player* "media_player.top_room")

(defn speak [message]
  (play-urls
   *media-player*
   [[(:url (ha-call
            "/tts_get_url"
            {:platform "google_translate"
             :message message}))
     message]]))

(defmulti handle-command (fn [config c] (keyword (namespace (:command c)))))

(defmethod handle-command :ha [config {:keys [command args]}]
  (let [service (name command)]
    (ha-call (str "/services/" (string/replace service #"\." "/"))
             (merge (:ha/args *config*) args))))

(defmethod handle-command :default [config c]
  (println "Unknown command" c))

(defn run [{:keys [grammar config]}]
  (binding [*config* (edn/read (java.io.PushbackReader.
                                (io/reader (io/as-file config))))])
  (let [parser (load-grammar grammar)]
    (loop []
      (let [line (read-line)]
        (when (string/starts-with? line "COM ")
          (let [result (parser (subs line 4))]
            (if (insta/failure? result)
              (println "nope" line result)
              (handle-command result)))))
      (recur))))


(defn- main [grammar config]
  (run {:grammar grammar :config config}))
