(ns torosalto.test-runner
  (:require [clojure.test :as test]
            [torosalto.api.openai-test]
            [torosalto.notation-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (test/run-tests 'torosalto.api.openai-test
                                           'torosalto.notation-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
