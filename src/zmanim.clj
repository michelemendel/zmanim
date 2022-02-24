(ns zmanim
  "
  Java-time
  https://github.com/dm3/clojure.java-time
  Java-time API
  http://dm3.github.io/clojure.java-time
  "
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [java-time :as jt]
            [kosher-java :as k])
  (:import (java.util TimeZone)
           (java.text SimpleDateFormat)))

(pp/pprint "zmanim")

;; --------------------------------------------------------------------------------
;; Date utils

(defn- inst->local-date
  ([inst]
   (jt/local-date inst (jt/zone-id)))
  ([y m d]
   (jt/local-date y m d)))

(defn- month-property
  [date]
  (jt/property date :day-of-month))

(defn- nof-day-in-month
  ([inst]
   (->> inst
        inst->local-date
        month-property
        jt/max-value))
  ([y m]
   (nof-day-in-month (inst->local-date y m 1))))

(defn- all-days-by-year
  [y]
  (for [m (range 1 13)
        d (range 1 (inc (nof-day-in-month y m)))]
    [y m d]))

(defn- all-days-by-year-month
  [y m]
  (for [d (range 1 (inc (nof-day-in-month y m)))]
    [y m d]))

;; --------------------------------------------------------------------------------
;; Presentation

;;http://tutorials.jenkov.com/java-date-time/parsing-formatting-dates.html
(defn date-pp
  [{:keys [algorithm location lat long date hebrew-date title->times]}]
  (let [frmt-day (SimpleDateFormat. "EEE yyyy-MM-dd")
        frmt-time (SimpleDateFormat. "HH:mm:ss")
        max-length (->> title->times vals
                        (map #(count (first %)))
                        (apply max))
        zmanim (fn [[idx [k v]]]
                 (format (str "%2d. %-" max-length "s %s\n") idx k (when v (.format frmt-time v))))]
    (str (format "%s, %s, %s\n" location (k/frmt-full-heb-date hebrew-date) (.format frmt-day date))
         (format "lat: %s, long: %s\n" lat long)
         (format "algorithm: %s\n" algorithm)
         (->> (map zmanim title->times)
              (str/join "")))))

(defn- columns-widths
  [titles]
  ;; The width for the day and heb-date is added to the start of the vector
  (into [6 12] (map #(max (count %) 9) titles)))

(defn- columns-frmt-pp
  [{:keys [columns-titles] :as zmanim-by-dates}]
  (str/join "" (map #(str "%-" % "s ") (columns-widths columns-titles))))

(defn- columns-frmt-csv
  [{:keys [columns-titles] :as zmanim-by-dates}]
  (str/join "," (map (fn [_] (str "%-1s")) (columns-widths columns-titles))))

(defn table-row-title
  [frmt-str {:keys [location date hebrew-date lat long algorithm] :as meta-data} titles]
  (let [year (.format (SimpleDateFormat. "yyyy Z z") date)
        heb-year (k/frmt-heb-year hebrew-date)]
    (str (format "%s, %s, %s\n" location heb-year year)
         (format "lat: %s, long: %s (algorithm: %s)\n" lat long algorithm)
         (apply format frmt-str (into ["date" "heb-date"] titles))
         "\n")))

(defn table-row
  [frmt-str {:keys [date hebrew-date title->times]}]
  (let [frmt-day (SimpleDateFormat. "MM-dd")
        frmt-time (SimpleDateFormat. "HH:mm:ss")
        date-MM-dd (.format frmt-day date)
        hebrew-date-MMMM-dd (k/frmt-heb-month-day hebrew-date)
        times (->> title->times
                   vals
                   (map #(when (second %)
                           (.format frmt-time (second %))))
                   (into [date-MM-dd hebrew-date-MMMM-dd]))
        ]
    (str (apply format frmt-str times) "\n")))

(defn- dates-pretty-print
  [columns-frmt {:keys [zmanim columns-titles meta-data] :as zmanim-by-dates}]
  (let [title (table-row-title columns-frmt meta-data columns-titles)
        times (->> zmanim
                   (map #(table-row columns-frmt %))
                   (str/join ""))]
    (str title times)))

(defn- dates-pp
  [zmanim-by-dates]
  (let [console-pp-frmt (columns-frmt-pp zmanim-by-dates)]
    (dates-pretty-print console-pp-frmt zmanim-by-dates)))

(defn dates-csv
  [zmanim-by-dates]
  (let [csv-frmt (columns-frmt-csv zmanim-by-dates)]
    (dates-pretty-print csv-frmt (->> zmanim-by-dates))))


;; --------------------------------------------------------------------------------
;; Location specific data for Oslo

;;The table from Benzy uses the coordinates of the shul:
;;59.9257381288325, 10.743053827519505
(def location-oslo {:locationName "Oslo, NO"
                    :latitude #_59.91387 59.92573
                    :longitude #_10.75225 10.74305
                    :elevation 0
                    :time-zone (TimeZone/getTimeZone "Europe/Oslo")})

(def shitot-oslo
  [["mishey.10.2°" k/misheyakir-10-2-deg]
   ["alot_72m" k/alos-72-min]
   ["sunrise" k/sunrise]
   ["sof_zman_shma_G" k/sof-zman-shma-gra] ;;3 shaot zmaniyot after sunrise
   ["sof_zman_tfila_G" k/sof-zman-tfila-gra]
   ["chatzos" k/chatzos ".getChatzos"]
   ["mincha_g.30m" k/mincha-gedola-30-min]
   ["mincha_k.G" k/mincha-ketana]
   ["plag_hamin.G" k/plag-hamincha]
   ["candles_G" k/candle-lighting]
   ["sunset" k/sunset]
   ["tz.kr.shma_bh" k/tzais-baal-hatanya]
   ["tz.motze_8.1°" k/tzais-geonim-8-1-deg]])

(def shitot-oslo-selected
  [
   ["mishey_10.2°" k/misheyakir-10-2-deg]
   ["alot_72m" k/alos-72-min]
   ["sunrise" k/sunrise]
   ;;["sof_zman_shma_G" k/sof-zman-shma-gra]
   ["sunset" k/sunset]
   ["tz.motze_8.1°" k/tzais-geonim-8-1-deg]
   ["tz.motze_72°" k/tzais-72-min-zmanis]
   ;;["sh.zm.mga" k/shaah-zmanis-mga]
   ;;["sh.zm.72m_zm" k/shaah-zmanis-72-min-zmanis]
   ])

;; --------------------------------------------------------------------------------
;; run app

;; Table
(do
  (let [year 2022
        ;;days (all-days-by-year year)
        days [[2022 1 1]
              [2022 3 1]
              [2022 5 1]
              [2022 5 19]
              [2022 5 20]
              [2022 5 30]
              [2022 6 1]
              [2022 6 20]
              [2022 6 21]
              [2022 6 22]
              [2022 6 23]
              ]
        ;;days [[2022 6 24]]
        zmanim-by-dates (k/zmanim-by-dates-and-location location-oslo shitot-oslo-selected days)
        filename (format "/Users/mendel/Downloads/zmanim_oslo_%s.txt" year)]
    (->> zmanim-by-dates
         dates-pp
         print
         ;;(spit filename)
         )))

;; Single day
(comment
 (clojure.pprint/pprint location-oslo)
 (->> [2022 2 21]
      (k/zmanim-by-date-and-location location-oslo shitot-oslo)
      date-pp
      ;;print
      ))

;; --------------------------------------------------------------------------------
;; sandbox

;; --------------------------------------------------------------------------------
;; misc

;; ----- Daf
#_(let [daf (YomiCalculator/getDafYomiBavli (JewishCalendar.))]
    {(.getMasechtaTransliterated daf)
     (.getDaf daf)}
    )




