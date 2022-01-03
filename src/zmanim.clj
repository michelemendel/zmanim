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
;; Presentation utils

;;http://tutorials.jenkov.com/java-date-time/parsing-formatting-dates.html
(defn present-key-val
  [{:keys [algorithm location date hebrew-date title->times]}]
  (let [frmt-day (SimpleDateFormat. "EEE yyyy-MM-dd")
        frmt-time (SimpleDateFormat. "HH:mm:ss")
        max-length (->> title->times vals
                        (map #(count (first %)))
                        (apply max))]
    (str (format "%s, %s, %s\n" location (k/frmt-full-heb-date hebrew-date) (.format frmt-day date))
         (format "algorithm: %s\n" algorithm)
         (reduce (fn [acc [idx [k v]]]
                   (str acc (format (str "%2d. %-" max-length "s %s\n")
                                    idx
                                    k
                                    (when v (.format frmt-time v)))))
                 "" title->times))))

(defn- col-widths
  [titles]
  ;; The width for the day and heb-date is added to the start of the vector
  (into [6 12] (map #(max (count %) 9) titles)))

(defn zmanim-table-row-title
  [frmt-str {:keys [location date hebrew-date titles]}]
  (let [year (.format (SimpleDateFormat. "yyyy Z z") date)
        heb-year (k/frmt-heb-year hebrew-date)]
    (str (format "%s, %s, %s\n" location heb-year year)
         (apply format frmt-str (into ["date" "heb-date"] titles))
         "\n")))

(defn zmanim-table-row
  [frmt-str {:keys [date hebrew-date title->times]}]
  (let [frmt-day (SimpleDateFormat. "MM-dd")
        frmt-time (SimpleDateFormat. "HH:mm:ss")
        day (.format frmt-day date)
        times (into [day (k/frmt-heb-month-day hebrew-date)] (->> title->times vals (map #(when (second %)
                                                   (.format frmt-time (second %))))))]
    (str (apply format frmt-str times) "\n")))

(defn zmanim-table
  [shitot calendar days is-csv?]
  (let [times-data (map #(k/get-times-data shitot calendar %) days)
        titles (->> times-data first :title->times vals (map first))
        meta-data (-> times-data first
                      (dissoc :title->times)
                      (assoc :titles titles))
        lengths (col-widths titles)
        frmt-str (if is-csv?
                   (str/join "," (map (fn [_] (str "%-1s")) lengths))
                   (str/join "" (map #(str "%-" % "s ") lengths)))
        title (zmanim-table-row-title frmt-str meta-data)
        times (reduce (fn [acc time]
                        (str acc (zmanim-table-row frmt-str time)))
                      "" times-data)]
    (str title (str/join "" times))))

;; --------------------------------------------------------------------------------
;; Location specific data for Oslo

(def location-oslo {:locationName "Oslo, NO"
                    :latitude 59.91387
                    :longitude 10.75225
                    :elevation 0
                    :time-zone (TimeZone/getTimeZone "Europe/Oslo")})

(def shitot-oslo
  {1 ["mishey.10.2°" k/misheyakir-10-2-deg]
   2 ["alot_72m" k/alos-72-min]
   3 ["sunrise" k/sunrise]
   4 ["sof_zman_shma_G" k/sof-zman-shma-gra]
   5 ["sof_zman_tfila_G" k/sof-zman-tfila-gra]
   6 ["chatzos" k/chatzos ".getChatzos"]
   7 ["mincha_g.30m" k/mincha-gedola-30-min]
   8 ["mincha_k.G" k/mincha-ketana]
   9 ["plag_hamin.G" k/plag-hamincha]
   10 ["candles_G" k/candle-lighting]
   11 ["sunset" k/sunset]
   12 ["tz.kr.shma_bh" k/tzais-baal-hatanya]
   13 ["tz.motze_8.1°" k/tzais-geonim-8-1-deg]})

(def shitot-oslo-kaluach-vs-java
  {1 ["mishey.11°" k/misheyakir-11-deg]
   2 ["alot_16.1°-kl" k/alos-16-1-deg]
   3 ["alot_72m" k/alos-72-min]
   4 ["tz.motze_8.5°-kl" k/tzais-geonim-8-5-deg]
   5 ["tz.motze_8.1°" k/tzais-geonim-8-1-deg]})

(def shitot-oslo-selected
  {1 ["mishe_11°" k/misheyakir-11-deg]
   2 ["alot_72m" k/alos-72-min]
   3 ["sunrise" k/sunrise]
   4 ["sunset" k/sunset]
   5 ["tzet_8.1°" k/tzais-geonim-8-1-deg]})


;; --------------------------------------------------------------------------------
;; run app

;; Table
(do
 (let [calendar (k/make-zmanim-cal-by-location (k/make-location location-oslo))
       year 2022
       table (zmanim-table shitot-oslo-selected
                           calendar
                           (all-days-by-year year)
                           true)
       filename (format "/Users/mendel/Downloads/zmanim_oslo_%s.txt" year)]
   ;;(print table)
   (spit filename table)
   ))

;; Single day
(comment
 (let [calendar (k/make-zmanim-cal-by-location (k/make-location location-oslo))]
   ;;(print (present-key-val (k/get-times-data shitot-oslo calendar [2022 6 24])))
   ;;(print (present-key-val (k/get-times-data shitot-oslo calendar)))
   (k/get-times-data shitot-oslo calendar [2022 6 24])
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




