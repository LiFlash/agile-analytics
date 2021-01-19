(ns agile-stats.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [java-time :as time]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-s" "--sprint-length WEEKS" "Number of weeks per Sprint"
    :default 2
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0) "Must be positive"]]
   ["-n" "--nr-of-sprints SPRINTS" "Number of sprints in the past to calculate the values for"
    :default 12
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0) "Must be positive"]]
   ["-l" "--last-sprint-date DATE" "The date of the last sprint change in the format yyyy-mm-dd ."
    :default (time/local-date)
    :default-desc "Today"
    :parse-fn #(time/local-date %)]
   ["-f" "--date-format FORMAT" "The format (without the time) of dates in the input file."
    :default "dd.MM.yyyy"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Agile Stats: Uses your existing backlog to help analyzing the past and to find
improvements for making the future more predictable."
        ""
        "Usage: astats [options] input-file output-file"
        ""
        "Input-file is a .csv file containing the following columns in this order:"
        "Issue Type, Issue Key, Issue Id, Summary, Story Points, Created (Date), Resolved (Date), Status"
        ""
        "The output is a .csv file containing the following values for every sprint:"
        "  Rolling Avg. Story Size - Avg. size (story points) of all stories resolved in the month before"
        "  Calculated Backlog Size  - Nr. of known issues multiplied by the average story size"
        "  Actual Backlog Size     - Sum of story points for stories that were already known (created before) and are estimated by today"
        "  Backlog Size by Today   - Sum of all story points that were estimated after the given sprint date. This also includes stories that were not created at a given date but are known today."
        "  #Open Issues            - Nr. of known open issues"
        "  Rolling Velocity        - Avg. velocity for the last three sprints"
        "  Current Velocity        - Actual Velocity in the last sprint"
        "  Burndowns               - Burndown for each sprint using the calculated backlog size and average velocity"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (= 2 (count arguments))
      {:in-file (first arguments), :out-file (second arguments),
       :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  ;; (System/exit status)
  )


(defn median [ns]
  (if (empty? ns)
    0
    (let [ns (sort ns)
          cnt (count ns)
          mid (bit-shift-right cnt 1)]
      (if (odd? cnt)
        (nth ns mid)
        (/ (+ (nth ns mid) (nth ns (dec mid))) 2)))))

(defn parse-date [date-string date-format]
  (let [date-string (-> date-string
                        (clojure.string/split #" ")
                        first)]
    (time/local-date date-format date-string)))


(defn parse-line [[type key id summary points created resolved status] date-format]
  {:type type
   :key key
   :id id
   :summary summary
   :points (or (and (not= "" points)
                    (Float/parseFloat points))
               0)
   :created (parse-date created date-format)
   :resolved (or (and (not= "" resolved)
                      (parse-date resolved date-format))
                 nil)
   :status status})

(defn read-stories [file-path date-format]
  (with-open [file (clojure.java.io/reader file-path)]
    (->> (csv/read-csv (slurp file))
         rest
         (map #(parse-line % date-format)))))

(defn create-burndown-matrix [burndowns dates]
  (let [date-map (reduce #(assoc % %2 "") {} dates)
        date-map-fn (fnil assoc date-map)]
    (loop [dates dates
           burndowns burndowns
           result {}]
      (if (empty? dates)
        result
        (let [date (first dates)
              result (reduce (fn [result [sprint-date val]]
                               (let[current-date-map (or (get result sprint-date)
                                                         date-map)]
                                 (update result sprint-date date-map-fn date val)))
                             result (first burndowns))]
          (recur (rest dates)
                 (rest burndowns)
                 result))))))

(defn prepare-burndowns
  "Input is a vector of burndwons, which each are vectors of date-value-tuples:
  [[[date value] [date value]]
   [[date value] [date value]]]

  Output is a vector of vectors each containing a date as the first value and a list of values:
  [[date value value value]
   [date value value value value]]"
  [burndowns dates]
  ;; creating a matrix matching each sprint-date to the values at the given
  ;; dates, i.e which value did the backlog-size have at a given date?
  (let [matrix (create-burndown-matrix burndowns dates)
        sprint-dates (sort-by first time/before?
                                    (into [] matrix))]
    (mapv (fn [sprint-date]
            (let [vals (->> sprint-date
                            second
                            (into [])
                            (sort-by first time/before?))]
              (into [(first sprint-date)] (mapv second vals))))
          sprint-dates)))

(prepare-burndowns [[[(time/local-date) "value"]]
                    [[(time/local-date) "value2"]
                     [(time/local-date 2018 10 2) "value3"]]]
                   [(time/local-date)
                    (time/local-date 2018 10 2)])

(defn write-stats [file-path stats]
  (let [dates (:date stats)
        burndowns (prepare-burndowns (:burndown stats) dates)]
    (with-open [writer (clojure.java.io/writer file-path)]
      (csv/write-csv writer
                     (into [(into ["Date"] (:date stats))
                            (into ["Rolling Avg. Story Size"] (:avg-size stats))
                            (into ["Calculated Backlog Size"] (:est-backlog-size stats))
                            (into ["Actual Backlog Size"] (:actual-backlog-size stats))
                            (into ["Backlog Size by today"] (:later-backlog-size stats))
                            (into ["#Open Issues"] (:open-issues stats))
                            (into ["Rolling Velocity"](:velocity stats))
                            (into ["Current Velocity"] (:actual-velocity stats))
                            [""]
                            (into [""] dates)]
                           burndowns)))))

(defn apply-filters [stories filters]
  (reduce #(filter %2 %1) stories filters))

(defn created-before? [date]
  (fn [item]
    (time/before? (:created item) (time/plus date (time/days 1)))))

(defn created-after? [date]
  (fn [item]
    (time/before? date (:created item))))

(defn resolved?
  ([] #(resolved? %))
  ([story]
   (:resolved story)))

(defn unresolved?
  ([] #(unresolved? %))
  ([story] (not (:resolved story))))

(defn resolved-after? [date]
  (fn [item]
    (let [resolved (:resolved item)]
      (or (nil? resolved)
          (time/before? date resolved)))))

(defn resolved-before? [date]
  (fn [item]
    (let [resolved (:resolved item)]
      (and resolved
           (time/before? resolved (time/plus date (time/days 1)))))))

(defn resolved-between? [start end]
  (fn [item]
    (and ((resolved-after? start) item)
         ((resolved-before? end) item))))

(defn estimated?
  ([] #(estimated? %))
  ([story] (< 0 (:points story))))

(defn unestimated?
  ([] #(unestimated? %))
  ([story] (not (estimated? story))))

(defn open-issues [stories date]
  (apply-filters stories [(created-before? date) (resolved-after? date)]))

(defn story-points [stories]
  (reduce #(+ %1 (or (:points %2) 0)) 0 stories))

(defn avg-estimation [stories date months]
  (let [start-date (time/minus date (time/months months))
        estimated-stories (apply-filters stories [(resolved-before? date) (resolved-after? start-date) estimated?])]
    (if (empty? estimated-stories)
      0
      (/ (story-points estimated-stories) (count estimated-stories)))))

(defn est-backlog-size
  "Estimates the backlog size (sum of story points for open stories) at a
  specific date. Only stories that were already known and open, i.e. not
  resolved at 'date' are taken into account.
  The size of the stories is estimated by the size of already finished and
  estimated stories.
  Params:
  stories - The stories to calc the backlog size with
  date - The date the backlog size should be estimated for; default: today
  months - The time frame to calculate the avg story size with. That is,
  only finished and estimated stories in these months before date will count
  for the story point estimation; default: 2"
  ([stories] (est-backlog-size stories (time/local-date)))
  ([stories date] (est-backlog-size stories date 2))
  ([stories date months] (Math/round (* 1.0
                                        (count (open-issues stories date))
                                        (avg-estimation stories date months)))))

(defn actual-backlog-size [stories date]
  (-> stories
      (open-issues date)
      story-points
      (* 1.0)
      Math/round)
  ;; (let [stories (open-issues stories date)]
  ;;   (Math/round (* 1.0 (story-points stories))))
  )

(defn later-backlog-size [stories date]
  (-> stories
      (apply-filters [(resolved-after? date)])
      story-points
      (* 1.0)
      Math/round))

(defn nr-of-sprints
  "calculates the number of sprints in which the given stories were resolved.
  sprint-length - The nr of days per sprint"
  [stories sprint-length]
  (let [stories (->> stories
                     (filter resolved?)
                     (sort-by :resolved time/before?))
        min-date (-> stories first :resolved)
        max-date (-> stories last :resolved)
        sprints (->> sprint-length
                     (/ (time/time-between min-date max-date :days))
                     double
                     Math/round)]
    (or (and (> sprints 1) sprints)
        1)))

(defn velocity
  "stories - The stories to calc the velocity with
  date - The date the velocity should be calculated for, i.e. what velocity did
  we have that day?; default: today
  sprint-length - The nr of weeks per sprint; default: 2
  nr-sprints - The nr of sprints (before date) to calculate the avg velocity
  for; default: 3"
  ([stories] (velocity stories (time/local-date)))
  ([stories date] (velocity stories date 3))
  ([stories date nr-sprints] (velocity stories date nr-sprints 2))
  ([stories date nr-sprints sprint-length]
   (let [start-date (time/minus date (time/weeks (* sprint-length nr-sprints)))
         stories (apply-filters stories
                                [(resolved-before? date)
                                 (resolved-after? start-date)])]
     (Math/round (* 1.0 (/ (story-points stories) nr-sprints))))))

(defn burndown [backlog-size velocity sprint-length date]
  (loop [new-size backlog-size
         new-date date
         result []]
    (if (< 0 new-size)
      (recur  (Math/round (* 1.0 (- new-size velocity)))
              (time/plus new-date (time/weeks sprint-length))
              (conj result [new-date new-size]))
      (conj result [new-date 0]))))

(defn rolling-values [stories date nr-sprints sprint-length]
  (let [months (-> nr-sprints range reverse)]
    (reduce #(let [date (time/minus date (time/weeks (* sprint-length %2)))
                   avg-story-size (avg-estimation stories date 1)
                   rolling-velocity (velocity stories date)
                   est-backlog-size (est-backlog-size stories date 1)]
               (-> %
                   (update :date conj date)
                   (update :velocity conj rolling-velocity)
                   (update :actual-velocity conj (velocity stories date 1 2))
                   (update :avg-size conj (-> avg-story-size (* 10.0) Math/round (/ 10.0)))
                   (update :est-backlog-size conj est-backlog-size)
                   (update :actual-backlog-size conj (actual-backlog-size stories date))
                   (update :later-backlog-size conj (later-backlog-size stories date))
                   (update :open-issues conj (count (open-issues stories date)))
                   (update :burndown conj (burndown est-backlog-size rolling-velocity 2 date))))
            {:date [], :est-backlog-size [], :actual-backlog-size [], :later-backlog-size [],
             :open-issues [], :velocity [], :actual-velocity [],
             :burndown [], :avg-size []}
            months)))


;; (def stories (read-stories "/media/sf_configs/stories.csv"))

;; (est-backlog-size stories (time/local-date 2018 11 1) 3)

;; (velocity stories (time/local-date 2018 9 1) 3 2)

;; (burndown 50 10 1 (time/local-date))

;; (write-stats "/media/sf_configs/stats.csv" (rolling-values ;(apply-filters stories [estimated?])
;;                                             stories
;;                                             20 2))

(defn start [args]
  (let [{:keys [in-file out-file options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [{:keys [date-format last-sprint-date sprint-length nr-of-sprints]} options
            stories (read-stories in-file date-format)
            stats (rolling-values stories
                                  last-sprint-date
                                  nr-of-sprints
                                  sprint-length)]
        (println "Creating stats with the following parameters:")
        (println "sprint-length: " sprint-length)
        (println "nr-of-sprints: " nr-of-sprints)
        (println "date-format: " date-format)
        (println "last-sprint-date: " (time/format date-format last-sprint-date))

        (write-stats out-file stats)
        (println "\nWrote stats to " out-file)))))

(defn -main [& args]
  (start args))

;(start '("-h"))
;(start '("-f" "dd/MMM/yy" "/media/sf_configs/stories.csv" "/media/sf_configs/stats.csv"))
;(start '("--help"))
