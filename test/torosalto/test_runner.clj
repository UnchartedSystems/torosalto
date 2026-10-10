(ns torosalto.test-runner
  (:require [clojure.test :as test]
            [torosalto.notation-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (test/run-tests 'torosalto.notation-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
