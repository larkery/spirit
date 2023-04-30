(ns spirit.grammar
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clj-fuzzy.metrics :refer [jaro]]
            [clojure.tools.logging :as log]))

(def stopwords (set (string/split-lines (slurp (io/resource "stopwords.txt")))))
(def metagrammar (insta/parser (io/resource "metagrammar.g")))
(def builtins    (insta/parser (io/resource "builtins.g")))
(def word-number-re
  (let [digit "(one|two|three|four|five|six|seven|eight|nine)"
        ten   "(ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen)"
        tens  "(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)"
        small (format "(^|[^a-z])((?<tens>%s)( +(?<tendigit>%s))?|(?<digit>%s)|(?<ten>%s))($|[^a-z])" tens digit digit ten)
        ]
    (re-pattern small)))

(def digit-values
  (let [fwd
        {"one"     1  "two"       2  "three"    3  "four"     4  "five"    5
         "six"     6  "seven"     7  "eight"    8  "nine"     9  "ten"     10
         "eleven"  11 "twelve"    12 "thirteen" 13 "fourteen" 14 "fifteen" 15
         "sixteen" 16 "seventeen" 17 "eighteen" 18 "nineteen" 19 "twenty"  20
         "thirty"  30 "forty"     40 "fifty"    50 "sixty"    60 "seventy" 70
         "eighty"  80 "ninety"    90}]
    (merge fwd (set/map-invert fwd))))

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
          (.appendReplacement matcher sb (str " " rep " "))
          (recur))
        (do (.appendTail matcher sb)
            (.toString sb))))))

(let [stopword-regex (re-pattern
                      (str "(^| +)("
                           (string/join "|" stopwords)
                           ")($| +)"
                           ))]
  (defn normalize [s]
    (-> s
        (string/replace #"[(\[].+?[)\]]" "")
        (string/replace #"[,.?! ]+" " ")
        (replace-word-numbers)
        (string/replace #"%" " percent")
        (string/lower-case)
        (string/replace stopword-regex " ")
        (string/replace #" +" " ")
        (string/trim))))

(defn parse-with-fuzz [g s n]
  (let [s (normalize s)
        r (g s)]
    (if (insta/failure? r)
      (if (> n 4) r
          (let [{:keys [index reason]} (insta/get-failure r)
                up-to  (subs s 0 index)
                rest-s (subs s index)
                [w rest-s] (string/split rest-s #" " 2)
                options
                (->> reason
                     (filter (comp #{:string} :tag))
                     (map :expecting)
                     (map (fn find-l [e] [(- 1.0 (jaro w e)) e]))
                     (filter (fn filter-l [%] (< (first %) 0.2)))
                     (sort))
                ]
            (println "PARSE" s "FAIL" options)
            (or (first (keep
                        (fn keep-parses [[_ e]]
                          (let [s (str up-to " " e " " rest-s)
                                _ (println "RETRY" s)
                                r (parse-with-fuzz g s (inc n))]
                            
                            (when-not (insta/failure? r) r)))
                        options))
                r)))
      r)))



(defn load-grammar [filename]
  (let [g (metagrammar (slurp (io/as-file filename)))
        _ (when (insta/failure? g) (log/error "Parsing" filename g))
        wrap-strings #(keep (fn wrap-string [i]
                              (cond (stopwords i) nil
                                    (string? i)
                                    (c/cat (c/hide (c/regexp #"\s*"))
                                           (c/string i)
                                           (c/hide (c/regexp #"\s*")))
                                    :else i))
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
                     {:integer  read-string
                      :hours    (fn [h & [m]] (* 60 (+ (or m 0) (* 60 h))))
                      :minutes  (fn [m] (* 60 m))
                      :seconds  identity
                      :duration identity
                      :time     (fn [& [a b c]]
                                  (let [mins
                                        (+ a
                                           (cond
                                             (number? b) b
                                             (= b "pm") (* 12 60)
                                             :else 0)
                                           (if (= c "pm") (* 12 60) 0))]
                                    (format "%02d:%02d:00"
                                            (int (/ mins 60))
                                            (int (mod mins 60)))))
                      :hour_of_day (fn [i] (* i 60))
                      :minute_of_hour identity
                      :minute_prefix (fn [amount direction]
                                        (let [amount
                                              (case amount
                                                "quarter" 15
                                                "half" 30
                                                amount)]
                                          (case direction
                                            "to"  (- amount)
                                            "past" amount)))
                      :root     (fn root [hs]
                                  {:command (first hs)
                                   :args    (into {} (remove string? (rest hs)))})})

                    (parse-with-fuzz parser line 0))]

        (-> (update result :args (fn merge-args [%]
                                   (merge (command-args (:command result)) %)))
            (assoc :command (commands (:command result))))
        ))))

(defn detect-wakeword [command]
  (when command
    (let [command (normalize command)
          words (string/split command #" +")
          wake-words (keep-indexed (fn [i w]
                                     (let [l (jaro w "spirit")]
                                       (when (> l 0.75) [l i])))
                                   words)
          wake-index (second (first (sort-by first wake-words)))]
      (when wake-index  (string/join " " (drop (inc wake-index) words))))))

(comment
  (builtins "20 past 9" :start :time)
  (let [g (load-grammar "test.grammar")]
    (g "set alarm for 8")
    )
  )
