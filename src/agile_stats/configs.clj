(ns agile-stats.configs
  (:require [java-time :as t]))
;;TODO Add status-categories to the individual configs itself (Adjust all places, where status-categories is used)
(def ds { ;:renew-db true
         :sprint-end-date (t/offset-date-time 2021 7 16)
         :update-date (t/offset-date-time 2020 11 1) ;(t/minus (t/offset-date-time) (t/weeks 15))
         :sprint-length 2
         :nr-sprints 8
         :storage-file "ds-issues.edn"
         :stats-file "ds-stats.csv"
         :base-url "https://digistore.atlassian.net"
         :issue-query "project = DS AND type not in (\"Test Execution\",
 \"Test Plan\", \"Test Set\", \"Xray Test\",
 \"Sub Test Execution\", Precondition, Sub-Bug, Sub-Task, Epic, Bug) AND labels in (DS_Frontend,DS_Backend) AND labels not in (nf)"})

(def ds-bugs { ;:renew-db true
              :sprint-end-date (t/offset-date-time 2021 6 4)
              :update-date (t/offset-date-time 2020 11 1) ;(t/minus (t/offset-date-time 2021 4 23) (t/weeks 14))
              :sprint-length 2
              :nr-sprints 10
              :storage-file "ds-bugs.edn"
              :stats-file "ds-bug-stats.csv"
              :base-url "https://digistore.atlassian.net"
              :issue-query "project = DS AND issuetype = Bug AND labels in (DS_Frontend,DS_Backend) AND labels not in (nf)"})

(def cch { ;:renew-db true
          :sprint-end-date (t/offset-date-time 2021 6 22)
          :sprint-length 2
          :nr-sprints 6
          :update-date (t/offset-date-time 2021 4 1) ;(t/minus (t/offset-date-time) (t/weeks 15))
          :storage-file "cch-issues.edn"
          :stats-file "cch-stats.csv"
          :base-url "https://digistore.atlassian.net"
          :issue-query "project in (OCB,PGB) AND issuetype in (Task, Story) AND (labels is empty or labels not in (nf))"})

(def oma { ;:renew-db true
          :sprint-end-date (t/offset-date-time 2021 6 8)
          :sprint-length 2
          :nr-sprints 14
          :update-date (t/offset-date-time 2021 1 1) ;(t/minus (t/offset-date-time) (t/weeks 15))
          :storage-file "oma-issues.edn"
          :stats-file "oma-stats.csv"
          :base-url "https://digistore.atlassian.net"
          :issue-query "project in (OMA) AND issuetype in (Task, Story) AND (labels is empty or labels not in (nf))"})

(def base-url "https://digistore.atlassian.net/rest/api/2/")

(def status-categories {:todo #{"To Do" "Dependent" "Can be groomed" "To be defined" "Selected for dev"}
                        :wip #{"Blocked" "To Be Fixed" "Design Review" "In Progress" "R4 Code Review" "Code Review" "R4 Testing" "Testing" "R4 Merge" "Approval" "Merge" "R4 Release" "Release"}
                        :done #{"Done"}})
