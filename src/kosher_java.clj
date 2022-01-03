(ns kosher-java
  "
  Kosher Java
  https://search.maven.org/artifact/com.kosherjava/zmanim
  "
  (:require [clojure.pprint :as pp])
  (:import (com.kosherjava.zmanim ComplexZmanimCalendar AstronomicalCalendar)
           (com.kosherjava.zmanim.util GeoLocation)
           (com.kosherjava.zmanim.hebrewcalendar JewishDate HebrewDateFormatter)))

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

(defn frmt-full-heb-date
  [heb-date]
  (let [frmt (HebrewDateFormatter.)]
    (format "%s-%s-%s-%s"
            (.getJewishYear heb-date) (.getJewishMonth heb-date) (.formatMonth frmt heb-date) (.getJewishDayOfMonth heb-date))))

(defn frmt-heb-year
  [heb-date]
  (.getJewishYear heb-date))

(defn frmt-heb-month-day
  [heb-date]
  (let [frmt (HebrewDateFormatter.)]
    (format "%s-%s" (.formatMonth frmt heb-date) (.getJewishDayOfMonth heb-date))))

(defn get-times-data
  "cal - ZmanimCalendar or ComplexZmanimCalendar
   day - [year month day]"
  ([shitot cal]
   (let [heb-date (JewishDate. (.getCalendar cal))]
     {:location (->> cal .getGeoLocation .getLocationName)
      :lat (->> cal .getGeoLocation .getLatitude)
      :long (->> cal .getGeoLocation .getLongitude)
      :algorithm (->> cal .getAstronomicalCalculator .getCalculatorName)
      :date (->> (.getCalendar cal) .getTime) ;;used for presentation
      :hebrew-date (->> heb-date) #_(->> cal .getDate)
      :title->times (->> (map (fn [[idx [title shita]]]
                                [idx [title (shita cal)]])
                              shitot)
                         (into (sorted-map)))}))
  ([shitot cal day]
   (set-calendar-time cal day)
   (get-times-data shitot cal)))

(defn misheyakir-10-2-deg [cal] (.getMisheyakir10Point2Degrees cal))
(defn misheyakir-11-deg [cal] (.getMisheyakir11Degrees cal))
(defn alos-hashachar [cal] (.getAlosHashachar cal))
(defn alos-72-min [cal] (.getAlos72 cal))
(defn alos-16-1-deg [cal] (.getAlos16Point1Degrees cal))
(defn sunrise [cal] (.getSunrise cal))
(defn sof-zman-shma-gra [cal] (.getSofZmanShmaGRA cal))
(defn sof-zman-tfila-gra [cal] (.getSofZmanTfilaGRA cal))
(defn chatzos [cal] (.getChatzos cal))
(defn mincha-gedola-30-min [cal] (.getMinchaGedola30Minutes cal))
(defn mincha-ketana [cal] (.getMinchaKetana cal))
(defn plag-hamincha [cal] (.getPlagHamincha cal))
(defn candle-lighting [cal] (.getCandleLighting cal))
(defn sunset [cal] (.getSunset cal))
(defn tzais-baal-hatanya [cal] (.getTzaisBaalHatanya cal))
(defn shaah-zmanis-16-1-deg [cal] (.getShaahZmanis16Point1Degrees cal))
(defn tzais-geonim-8-1-deg [cal] (.getSunsetOffsetByDegrees cal (+ 8.1 AstronomicalCalendar/GEOMETRIC_ZENITH)))
(defn tzais-geonim-8-5-deg [cal] (.getTzaisGeonim8Point5Degrees cal))

