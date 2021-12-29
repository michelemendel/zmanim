(ns zmanim
  "
  Kosher Java
  https://search.maven.org/artifact/com.kosherjava/zmanim

  Java-time
  https://github.com/dm3/clojure.java-time
  Java-time API
  http://dm3.github.io/clojure.java-time
  "
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [java-time :as jt])
  (:import (com.kosherjava.zmanim ComplexZmanimCalendar AstronomicalCalendar)
           (com.kosherjava.zmanim.util GeoLocation)
           (java.util TimeZone)
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
;; Zmanim utils

(defn make-location
  [l]
  (GeoLocation. (:locationName l) (:latitude l) (:longitude l) (:elevation l) (:time-zone l)))

(defn make-zmanim-cal-by-location
  [location]
  (ComplexZmanimCalendar. location))

(defn set-calendar-time
  "Month is 1-based instead of zero, so don't use the built-in months, since they are zero-based"
  [cal [year month day]]
  (.set (.getCalendar cal) year (dec month) day))

(defn get-times-data
  ([shitot cal]
   {:location (->> cal .getGeoLocation .getLocationName)
    :algorithm (.getCalculatorName (.getAstronomicalCalculator cal))
    :sunrise (.getSunrise cal)
    :title->times (->> (shitot cal) (into (sorted-map)))})
  ([shitot cal day]
   (set-calendar-time cal day)
   (get-times-data shitot cal)))

;; --------------------------------------------------------------------------------
;; Presentation utils

;;http://tutorials.jenkov.com/java-date-time/parsing-formatting-dates.html
(defn present-key-val
  [{:keys [algorithm location sunrise title->times]}]
  (let [df1 (SimpleDateFormat. "EEEE yyyy-MM-dd Z z")
        df2 (SimpleDateFormat. "HH:mm:ss")
        max-length (->> title->times vals
                        (map #(count (first %)))
                        (apply max))]
    (print (format "%s, %s\n" location (.format df1 sunrise)))
    ;;(print (format "algorithm: %s\n" algorithm))
    (doseq [[idx [k v]] title->times]
      (print (format (str "%2d. %-" max-length "s %s\n")
                     idx
                     k
                     (when v (.format df2 v)))))))

(defn- col-widths
  [titles]
  ;; The width for the day is added to the start of the vector
  (conj (map #(max (count %) 9) titles) 6))

(defn zmanim-table-row-title
  [{:keys [location sunrise title->times]}]
  (let [tf (SimpleDateFormat. "yyyy Z z")
        titles (->> title->times vals (map first))
        lengths (col-widths titles)
        frmt-str (str/join "" (map #(str "%-" % "s ") lengths))]
    (str (format "%s, %s\n" location (.format tf sunrise))
         (apply format frmt-str (conj titles "day"))
         "\n")))

(defn zmanim-table-row
  [{:keys [sunrise title->times]}]
  (let [frmt-day (SimpleDateFormat. "MM-dd")
        frmt-time (SimpleDateFormat. "HH:mm:ss")
        day (.format frmt-day sunrise)
        times (conj (->> title->times vals (map #(when (second %)
                                                   (.format frmt-time (second %)))))
                    day)
        lengths (->> title->times vals (map first) col-widths)
        frmt-str (str/join "" (map #(str "%-" % "s ") lengths))]
    (str (apply format frmt-str times) "\n")))

(defn zmanim-table
  [shitot calendar days]
  (let [times-data (map #(get-times-data shitot calendar %) days)
        title (zmanim-table-row-title (first times-data))
        times (reduce (fn [acc time]
                        (str acc (zmanim-table-row time)))
                      "" times-data)]
    (str title (str/join "" times))))


;; --------------------------------------------------------------------------------
;; Location specific data for Oslo

(def location-oslo {:locationName "Oslo, NO"
                    :latitude 59.91387
                    :longitude 10.75225
                    :elevation 0
                    :time-zone (TimeZone/getTimeZone "Europe/Oslo")})

(defn shitot-oslo
  [cal]
  {
   1 ["mishey.11°" (.getMisheyakir11Degrees cal)]
   2 ["alot_72m" (.getAlos72 cal)]
   3 ["sunrise" (.getSunrise cal)]
   4 ["sof_zman_shma_G" (.getSofZmanShmaGRA cal)]
   5 ["sof_zman_tfila_G" (.getSofZmanTfilaGRA cal)]
   6 ["chatzos" (.getChatzos cal)]
   7 ["mincha_g.30m" (.getMinchaGedola30Minutes cal)]
   8 ["mincha_k.G" (.getMinchaKetana cal)]
   9 ["plag_hamin.G" (.getPlagHamincha cal)]
   10 ["candles_G" (.getCandleLighting cal)]
   11 ["sunset" (.getSunset cal)]
   12 ["tz.kr.shma_bh" (.getTzaisBaalHatanya cal)]
   13 ["tz.motze_8.1°" (.getSunsetOffsetByDegrees cal (+ 8.1 AstronomicalCalendar/GEOMETRIC_ZENITH))]
   ;;14 ["shaah_zmanis_16.1°" (.getShaahZmanis16Point1Degrees cal)]
   })


;; --------------------------------------------------------------------------------
;; sandbox

;; Table
(comment
 (let [calendar (make-zmanim-cal-by-location (make-location location-oslo))
       table (zmanim-table shitot-oslo
                           calendar
                           (all-days-by-year 2022))
       filename "/Users/mendel/Downloads/zmanim_oslo_2022.txt"]
   (print table)
   ;;(spit filename table)
   ))

;; Single day
(comment
 (let [calendar (make-zmanim-cal-by-location (make-location location-oslo))]
   ;;(present-key-val (get-times shitot-oslo calendar [2022 6 24]))
   (present-key-val (get-times-data shitot-oslo calendar))
   ))











;; --------------------------------------------------------------------------------
;; misc

(defn present-kaluach
  [times]
  (println "\nKaluach")
  (println (:date times))
  (doseq [[k v] (:times times)]
    (print (format "%-40s %s\n" k v))))
(def kaluach {:date "Fri Dec 24 2021, 20 Teves 5782"
              :times (sorted-map "01. misheyakir (10.2°)" "7:41:32"
                                 "02. alot (72 min)" "8:07:22"
                                 "03. sunrise" "9:19:22"
                                 "04. sof zman shma (GRA)" "10:48:02"
                                 "05. sof zman tfila (GRA)" "11:17:35"
                                 "06. chatzot" "12:16:41"
                                 "07. mincha gedola" "12:31:27"
                                 "09. plag hamincha" "14:37:03"
                                 "10. candle lighting (18 min)" "14:55:59"
                                 "11. sunset" "15:13:59"
                                 "12. tzais geonim (8.5°)" "16:35:57"
                                 )})
;;(present-kaluach kaluach)



;; ----- Daf
#_(let [daf (YomiCalculator/getDafYomiBavli (JewishCalendar.))]
    {(.getMasechtaTransliterated daf)
     (.getDaf daf)}
    )




