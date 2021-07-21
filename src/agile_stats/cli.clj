(ns agile-stats.cli
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [java-time :as time]
            [agile-stats.core :as core]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [java-time :as t]))

(def cli-options
  [["-s" "--sprint-length WEEKS" "Number of weeks per Sprint"
    :default 2
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 %) "Must be positive"]]
   ["-n" "--nr-of-sprints SPRINTS" "Number of sprints in the past to calculate the values for"
    :default 12
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 %) "Must be positive"]]
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
        ;"Usage: agile-stats [options] config-file config"
        "Usage: agile-stats config-file config"
        ""
        "Config-file is an .edn file containing the configs to analyze one or more projects. All parameters can be given outside of a concrete config key and will be used as defaults"
        "An example config-file can look like this:"
        "{\"Config Key\" { "
        "   :sprint-end-date <End of the last iteration to calculate the stats for. Format: yyyy-MM-dd>"
        "   :storage-file <cache file to store the jira issue details in>"
        "   :stats-file <output .csv file to write the stats into>"
        "   :renew-db <clear cache and load everything from jira>"
        "   :mc-nr-issues <Nr of issues to calculate monte carlo percentiles for(when will they be done?)>"
        "   :mc-days <Nr of days to calculate the monte carlo percentiles for (how many issues will be done?)>"
        "   :issue-query <jql query to filter for the relevant issues,e.g.\"project=xxxx and issuetype=story\" " "project = DS AND type not in (\"Test Execution\",
 \"Test Plan\", \"Test Set\", \"Xray Test\",
 \"Sub Test Execution\", Precondition, Sub-Bug, Sub-Task, Epic, Bug) AND labels in (DS_Frontend,DS_Backend) AND labels not in (nf)"
        "   }"
        ":base-url <basic url to the jira instance> e.g. \"https://xxxxx.atlassian.net\""
        ":basic-auth {"
        "   :user <login name for jira instance>"
        "   :token <jira token to authorize with"
        "   }"
        ":sprint-length <number of weeks per iteration>"
        ":nr-sprints <nr of sprints to go back (starting from sprint-end-date)"
        "}"
;;        ""
;;        "Options:"
;;        options-summary
        ]
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
      {:config-file (first arguments), :config (second arguments),
       :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  ;; (System/exit status)
  )

(defn start [args]
  (let [{:keys [config-file config options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [configs (clojure.edn/read-string
                     (slurp config-file))
            global-params (select-keys configs [:base-url :basic-auth :sprint-end-date
                                        :storage-file :stats-file :mc-nr-issues
                                        :mc-nr-days :issue-query :renew-db :status-categories])
            config (merge global-params (get configs config))
            end-date (time/local-date (:sprint-end-date config))
            end-date (apply time/offset-date-time (time/as end-date :year :month-of-year :day-of-month))
            config (assoc config :sprint-end-date end-date)]
        (println (get configs config))
        (core/update-stats config)
        )
      ;; (let [{:keys [date-format last-sprint-date sprint-length nr-of-sprints]} options
      ;;       ;stories nil;(read-stories in-file date-format)
      ;;       ;stats ;; (rolling-values stories
      ;;             ;;                 last-sprint-date
      ;;             ;;                 nr-of-sprints
      ;;             ;;                 sprint-length)
      ;;       ]
      ;;   (println "Creating stats with the following parameters:")
      ;;   (println "sprint-length: " sprint-length)
      ;;   (println "nr-of-sprints: " nr-of-sprints)
      ;;   (println "date-format: " date-format)
      ;;   (println "last-sprint-date: " (time/format date-format last-sprint-date))

      ;;   ;(write-stats out-file stats)
      ;;   (println "\nWrote stats to " out-file))
      )))



(defn -main [& args]
  (start args))

;(start '("-h"))
;(start '("-f" "dd/MMM/yy" "/media/sf_configs/stories.csv" "/media/sf_configs/stats.csv"))
;(start '("--help"))
(comment (ns agile-stats.cli
           (:require [cli-example.server :as server]
                     [clojure.string :as string]
                     [clojure.tools.cli :refer [parse-opts]])
           (:import (java.net InetAddress))
           (:gen-class))

         (def cli-options
           [;; First three strings describe a short-option, long-option with optional
            ;; example argument description, and a description. All three are optional
            ;; and positional.
            ["-p" "--port PORT" "Port number"
             :default 80
             :parse-fn #(Integer/parseInt %)
             :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
            ["-H" "--hostname HOST" "Remote host"
             :default (InetAddress/getByName "localhost")
             ;; Specify a string to output in the default column in the options summary
             ;; if the default value's string representation is very ugly
             :default-desc "localhost"
             :parse-fn #(InetAddress/getByName %)]
            ;; If no required argument description is given, the option is assumed to
            ;; be a boolean option defaulting to nil
            [nil "--detach" "Detach from controlling process"]
            ["-v" nil "Verbosity level; may be specified multiple times to increase value"
             ;; If no long-option is specified, an option :id must be given
             :id :verbosity
             :default 0
             ;; Use :update-fn to create non-idempotent options (:default is applied first)
             :update-fn inc]
            ;; A boolean option that can explicitly be set to false
            ["-d" "--[no-]daemon" "Daemonize the process" :default true]
            ["-h" "--help"]])

         ;; The :default values are applied first to options. Sometimes you might want
         ;; to apply default values after parsing is complete, or specifically to
         ;; compute a default value based on other option values in the map. For those
         ;; situations, you can use :default-fn to specify a function that is called
         ;; for any options that do not have a value after parsing is complete, and
         ;; which is passed the complete, parsed option map as it's single argument.
         ;; :default-fn (constantly 42) is effectively the same as :default 42 unless
         ;; you have a non-idempotent option (with :update-fn or :assoc-fn) -- in which
         ;; case any :default value is used as the initial option value rather than nil,
         ;; and :default-fn will be called to compute the final option value if none was
         ;; given on the command-line (thus, :default-fn can override :default)

         (defn usage [options-summary]
           (->> ["This is my program. There are many like it, but this one is mine."
                 ""
                 "Usage: program-name [options] action"
                 ""
                 "Options:"
                 options-summary
                 ""
                 "Actions:"
                 "  start    Start a new server"
                 "  stop     Stop an existing server"
                 "  status   Print a server's status"
                 ""
                 "Please refer to the manual page for more information."]
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
               (and (= 1 (count arguments))
                    (#{"start" "stop" "status"} (first arguments)))
               {:action (first arguments) :options options}
               :else ; failed custom validation => exit with usage summary
               {:exit-message (usage summary)})))

         (defn exit [status msg]
           (println msg)
           (System/exit status))

         (defn -main [& args]
           (let [{:keys [action options exit-message ok?]} (validate-args args)]
             (if exit-message
               (exit (if ok? 0 1) exit-message)
               (case action
                 "start"  (server/start! options)
                 "stop"   (server/stop! options)
                 "status" (server/status! options))))))
