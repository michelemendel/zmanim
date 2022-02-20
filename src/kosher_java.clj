(ns kosher-java
  "
  Kosher Java
  https://search.maven.org/artifact/com.kosherjava/zmanim
  "
  (:require)
  (:import (com.kosherjava.zmanim ComplexZmanimCalendar AstronomicalCalendar)
           (com.kosherjava.zmanim.util GeoLocation)
           (com.kosherjava.zmanim.hebrewcalendar JewishDate HebrewDateFormatter)))

(defn make-location
  [l]
  (GeoLocation. (:locationName l) (:latitude l) (:longitude l) (:elevation l) (:time-zone l)))

(defn zmanim-calendar-by-location
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

(defn- zmanim-by-date-and-calendar
  "calendar - ZmanimCalendar or ComplexZmanimCalendar"
  [calendar shitot]
  (let [heb-date (JewishDate. (.getCalendar calendar))]
    {:location (->> calendar .getGeoLocation .getLocationName)
     :lat (->> calendar .getGeoLocation .getLatitude)
     :long (->> calendar .getGeoLocation .getLongitude)
     :algorithm (->> calendar .getAstronomicalCalculator .getCalculatorName)
     :date (->> (.getCalendar calendar) .getTime) ;;used for presentation
     :hebrew-date (->> heb-date) #_(->> calendar .getDate)
     :title->times (->> (map-indexed (fn [idx [title shita]]
                                       [(inc idx) [title (shita calendar)]])
                                     shitot)
                        (into (sorted-map)))}))

(defn zmanim-by-today-and-location
  [location shitot]
  (let [calendar (zmanim-calendar-by-location (make-location location))]
    (zmanim-by-date-and-calendar calendar shitot)))

(defn zmanim-by-date-and-location
  "date - [year month day]"
  [location shitot date]
  (let [calendar (zmanim-calendar-by-location (make-location location))]
    (set-calendar-time calendar date)
    (zmanim-by-date-and-calendar calendar shitot)))

(defn zmanim-by-dates-and-location
  [location shitot days]
  (let [times-raw (map #(zmanim-by-date-and-location location shitot %) days)]
    {:columns-titles (->> times-raw first :title->times vals (map first))
     :zmanim (map #(select-keys % [:date :hebrew-date :title->times]) times-raw)
     :meta-data (-> times-raw first
                    (dissoc :title->times))}))

(def zenith AstronomicalCalendar/GEOMETRIC_ZENITH)

(defn misheyakir-10-2-deg [cal] (.getMisheyakir10Point2Degrees cal))
(defn misheyakir-by-deg [deg cal] (.getSunriseOffsetByDegrees cal (+ zenith deg)))
(defn misheyakir-11-deg [cal] (.getMisheyakir11Degrees cal))
(defn alos-hashachar [cal] (.getAlosHashachar cal))
(defn alos-72-min [cal] (.getAlos72 cal)) ;;kaluach Alot72eq
(defn alos-72-min-zmanis [cal] (.getAlos72Zmanis cal)) ;;kaluach Alot72prop
(defn alos-16-1-deg [cal] (.getAlos16Point1Degrees cal))
(defn sunrise [cal] (.getSunrise cal)) ;;hanetz
(defn sof-zman-shma-gra [cal] (.getSofZmanShmaGRA cal)) ;;3 shaot zmaniyot after sunrise
(defn sof-zman-tfila-gra [cal] (.getSofZmanTfilaGRA cal))
(defn chatzos [cal] (.getChatzos cal))
(defn mincha-gedola-30-min [cal] (.getMinchaGedola30Minutes cal))
(defn mincha-ketana [cal] (.getMinchaKetana cal))
(defn plag-hamincha [cal] (.getPlagHamincha cal))
(defn candle-lighting [cal] (.getCandleLighting cal))
(defn sunset [cal] (.getSunset cal))
(defn tzais-baal-hatanya [cal] (.getTzaisBaalHatanya cal))
(defn tzais-geonim-8-1-deg [cal] (.getSunsetOffsetByDegrees cal (+ zenith 8.1)))
(defn tzais-geonim-8-5-deg [cal] (.getTzaisGeonim8Point5Degrees cal))
(defn tzais-72-min [cal] (.getTzais72 cal))
(defn tzais-72-min-zmanis [cal] (.getTzais72Zmanis cal))

(defn shaah-zmanis-mga [cal] (.getShaahZmanisMGA cal))
(defn shaah-zmanis-gra [cal] (.getShaahZmanisGra cal))
(defn shaah-zmanis-16-1-deg [cal] (.getShaahZmanis16Point1Degrees cal))
(defn shaah-zmanis-72-min [cal] (.getShaahZmanis72Minutes cal))
(defn shaah-zmanis-72-min-zmanis [cal] (.getShaahZmanis72MinutesZmanis cal))

