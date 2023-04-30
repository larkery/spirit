(ns spirit.whisper
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]))

(def whisper-path (get (into {} (System/getenv)) "WHISPER_PATH" "main"))
(def whisper-args (map str (read-string (get (into {} (System/getenv)) "WHISPER_ARGS" "[]"))))
(defn run-whisper [wav-file]
  (:out (apply
         sh whisper-path
         (-> whisper-args
             (conj "-nt")
             (conj (.getCanonicalPath (io/as-file wav-file)))))))

