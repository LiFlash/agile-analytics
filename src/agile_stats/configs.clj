(ns agile-stats.configs
  (:require [java-time :as t]))

(def ds {:renew-db true
         :sprint-end-date (t/offset-date-time 2021 5 21)
         :update-date (t/offset-date-time 2020 11 1) ;(t/minus (t/offset-date-time) (t/weeks 15))
         :sprint-length 2
         :nr-sprints 14
         :storage-file "ds-issues.edn"
         :stats-file "ds-stats.csv"
         :base-url "https://digistore.atlassian.net/rest/api/2/"
         :issue-query "project = DS AND type not in (\"Test Execution\",
 \"Test Plan\", \"Test Set\", \"Xray Test\",
 \"Sub Test Execution\", Precondition, Sub-Bug, Sub-Task, Epic, Bug) AND labels in (DS_Frontend,DS_Backend) AND labels not in (nf)"})

(def ds-bugs { ;:renew-db true
              :sprint-end-date (t/offset-date-time 2021 5 21)
              :update-date (t/offset-date-time 2020 11 1) ;(t/minus (t/offset-date-time 2021 4 23) (t/weeks 14))
              :sprint-length 2
              :nr-sprints 10
              :storage-file "ds-bugs.edn"
              :stats-file "ds-bug-stats.csv"
              :base-url "https://digistore.atlassian.net/rest/api/2/"
              :issue-query "project = DS AND issuetype = Bug AND labels in (DS_Frontend,DS_Backend) AND labels not in (nf)"})

(def cch { ;:renew-db true
          :sprint-end-date (t/offset-date-time 2021 5 25)
          :sprint-length 2
          :nr-sprints 7
          :update-date (t/offset-date-time 2021 1 1) ;(t/minus (t/offset-date-time) (t/weeks 15))
          :storage-file "cch-issues.edn"
          :stats-file "cch-stats.csv"
          :base-url "https://digistore.atlassian.net/rest/api/2/"
          :issue-query "project in (OCB,PGB) AND issuetype in (Task, Story) AND (labels is empty or labels not in (nf))"})

(def base-url "https://digistore.atlassian.net/rest/api/2/")

(def status-categories {:todo #{"To Do" "Dependent" "Can be groomed" "To be defined" "Selected for dev"}
                        :wip #{"Blocked" "To Be Fixed" "In Progress" "R4 Code Review" "Code Review" "R4 Testing" "Testing" }
                        :done #{"R4 Merge" "Approval" "Merge" "R4 Release" "Release" "Done"}})
