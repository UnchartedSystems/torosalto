(ns torosalto.agent)

(def rules (slurp "rules.md"))
(def notation (slurp "notation.md"))

;; Open AI map.
{:text "4",
 :model "gpt-6.1-sol",
 :response-id
 "resp_01ef79a896c44c0d016ada6f4f27b887d0b7a1c7e69093946b",
 :usage
 {:attribution
  {:items
   {:msg_01ef79a896c44c0d016ada6f4f2e1487d09a572da388ee7a03
    {:cache_write_tokens 0,
     :cached_tokens 0,
     :content
     [{:cache_write_tokens 0,
       :cached_tokens 0,
       :input_tokens 18,
       :output_tokens 0}],
     :input_tokens 18,
     :output_tokens 0},
    :msg_01ef79a896c44c0d016ada6f503c5887d084394e9e8821e855
    {:cache_write_tokens 0,
     :cached_tokens 0,
     :content
     [{:cache_write_tokens 0,
       :cached_tokens 0,
       :input_tokens 2,
       :output_tokens 5}],
     :input_tokens 2,
     :output_tokens 5}}},
  :input_tokens 20,
  :input_tokens_details {:cache_write_tokens 0, :cached_tokens 0},
  :output_tokens 5,
  :output_tokens_details {:reasoning_tokens 0},
  :total_tokens 25}}
