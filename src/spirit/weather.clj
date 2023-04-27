(ns spirit.weather
  (:require [hato.client :as http]
            [clojure.string :as string])
  (:import [java.time Instant LocalTime]))

(def short-weather
  {
   "Clear"                               "Clear"
   "Sunny"                               "Sunny"
   "Partly cloudy"                       "Cloudy"
   "Cloudy"                              "Cloudy"
   "Overcast"                            "Cloudy"
   "Mist"                                "Misty"
   "Patchy rain possible"                "Rainy"
   "Patchy snow possible"                "Rainy"
   "Patchy sleet possible"               "Rainy"
   "Patchy freezing drizzle possible"    "Rainy"
   "Thundery outbreaks possible"         "Stormy"
   "Blowing snow"                        "Snowy"
   "Blizzard"                            "Snowy"
   "Fog"                                 "Fog"
   "Freezing fog"                        "Fog"
   "Patchy light drizzle"                "Rainy"
   "Light drizzle"                       "Rainy"
   "Freezing drizzle"                    "Rainy"
   "Heavy freezing drizzle"              "Rainy"
   "Patchy light rain"                   "Rainy"
   "Light rain"                          "Rainy"
   "Moderate rain at times"              "Rainy"
   "Moderate rain"                       "Rainy"
   "Heavy rain at times"                 "Rainy"
   "Heavy rain"                          "Rainy"
   "Light freezing rain"                 "Rainy"
   "Moderate or heavy freezing rain"     "Rainy"
   "Light sleet"                         "Rainy"
   "Moderate or heavy sleet"             "Rainy"
   "Patchy light snow"                   "Snowy"
   "Light snow"                          "Snowy"
   "Patchy moderate snow"                "Snowy"
   "Moderate snow"                       "Snowy"
   "Patchy heavy snow"                   "Snowy"
   "Heavy snow"                          "Snowy"
   "Ice pellets"                         "Ice"
   "Light rain shower"                   "Rainy"
   "Moderate or heavy rain shower"       "Rainy"
   "Torrential rain shower"              "Rainy"
   "Light sleet showers"                 "Rainy"
   "Moderate or heavy sleet showers"     "Rainy"
   "Light snow showers"                  "Snowy"
   "Moderate or heavy snow showers"      "Rainy"
   "Patchy light rain with thunder"      "Stormy"
   "Moderate or heavy rain with thunder" "Stormy" 
   "Patchy light snow with thunder"      "Stormy"
   "Moderate or heavy snow with thunder" "Stormy"
   }
  )

(defn- speak-list [items]
  (loop [b ""
         items items]
    (if (seq items)
      (recur
       (str b
            (cond
              (string/blank? b) ""
              (seq (rest items)) ", "
              :else " and ")
            (first items))
       
       (rest items))
      
      b)))

(defn- format-weather [weather soon?]
  (let [hourly (:hourly weather)]
    (if soon?
      (let [time-now (* 100 (.getHour (LocalTime/now)))
            next-weather (first (filter #(<= 0 (compare (Double/parseDouble (:time %)) time-now)) hourly))]
        (str (format "%s degrees, %s"
                     (:tempC next-weather)
                     (speak-list (map :value (:weatherDesc next-weather))))))
      
      (format "%s degrees, %s" (:avgtempC weather)
              (speak-list (set (map (comp short-weather :value) (mapcat :weatherDesc hourly)))))
      )))

(defn query-weather [day]
  (let [weather (-> (http/get "https://wttr.in/Bristol?format=j1"
                              {:accept :json
                               :as :json})
                    (:body)
                    (:weather)
                    (->> (sort-by :date)))]
    (str (case day
           0 "the outlook is "
           1 "tomorrow's forecast is "
           2 "the day after tomorrow's forecast is "
           "")
         
         (format-weather (nth weather day) (zero? day)))))

