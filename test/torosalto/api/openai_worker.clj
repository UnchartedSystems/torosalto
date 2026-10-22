(ns torosalto.api.openai-worker
  "Subprocess fixture for testing shared credentials with real JVM file locks."
  (:require [clojure.java.io :as io]
            [torosalto.api.openai :as openai]))

(defn -main [credentials root tag]
  (let [file (io/file credentials)
        directory (.getParentFile file)
        ready (io/file directory (str tag ".ready"))
        start (io/file directory "start")
        deadline (+ (System/nanoTime) 30000000000)]
    (spit ready "ready")
    (loop []
      (when-not (.exists start)
        (when (> (System/nanoTime) deadline)
          (throw (ex-info "Process test did not start." {})))
        (Thread/sleep 20)
        (recur)))
    (with-redefs-fn {#'openai/secrets-file file #'openai/issuer root #'openai/resource root}
      #(println (:text (openai/ask! {:model "test" :question tag}))))))
