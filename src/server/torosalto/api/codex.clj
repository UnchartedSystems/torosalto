(ns torosalto.api.codex
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]
            [clojure.data.json :as json]))

(def codex-args
  ["exec" "--json"
   "--model" "gpt-6.1-sol"
   "--ignore-user-config"
   "--sandbox" "read-only"
   "--disable" "shell_tool"
   "--disable" "apps"
   "--disable" "plugins"
   "--disable" "multi_agent"
   "-c" "web_search=\"disabled\""
   "-c" "model_reasoning_effort=\"medium\""
   "-c" "model_reasoning_summary=\"detailed\""
   "-c" "hide_agent_reasoning=false"
   "-c" "model_instructions_file=\"bench-instructions.txt\""
   "-c" "personality=\"none\""
   "-c" "project_doc_max_bytes=0"
   "--color" "never"
   "--skip-git-repo-check"])

(let [output (apply shell/sh "codex" (conj codex-args  "-" :in "What is the capital of Algeria"))]
  (->> (string/split-lines (:out output))
       (remove string/blank?)
       (mapv #(json/read-str % :key-fn keyword))))


(let [output (apply shell/sh "codex" (conj codex-args "resume" "01a15d50-774e-7e83-a651-63c597e99a54"  "-" :in "What question did I just ask you?"))]
  (->> (string/split-lines (:out output))
       (remove string/blank?)
       (mapv #(json/read-str % :key-fn keyword))))

;; Initial token usage for trivial questions is >6,000 tokens, even with these arguments.
;; Switching gears and setting up the responses API.
