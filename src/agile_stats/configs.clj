(ns agile-stats.configs
  (:require [java-time :as t]))

(def ds {:sprint-end-date (t/offset-date-time 2021 3 12)
         :update-date (t/offset-date-time 2020 12 1);(t/offset-date-time 2020 11 1)
         :sprint-length 2
         :nr-sprints 7
         :storage-file "ds-issues.edn"
         :stats-file "ds-stats.csv"
         :update-query "project = DS AND type not in (\"Test Execution\",
 \"Test Plan\", \"Test Set\", \"Xray Test\",
 \"Sub Test Execution\", Precondition, Sub-Bug, Sub-Task, Epic, Bug) AND labels in (DS_Frontend,DS_Backend)"})

(def cch {:sprint-end-date (t/offset-date-time 2021 3 16)
          :sprint-length 2
          :nr-sprints 7
          :update-date (t/offset-date-time 2020 12 1)
          :storage-file "cch-issues.edn"
          :stats-file "cch-stats.csv"
          :update-query "project in (OCB,PGB) AND issuetype in (Task, Story)"})

(def base-url "https://digistore.atlassian.net/rest/api/2/")

(def query-url (str base-url "search?jql="))

(def status-categories {:todo #{"To Do" "Dependent" "Can be groomed" "To be defined"}
                        :wip #{"Blocked" "To Be Fixed" "In Progress" "R4 Code Review" "Code Review" "R4 Testing" "Testing" }
                        :done #{"R4 Merge" "Approval" "Merge" "R4 Release" "Done"}})
