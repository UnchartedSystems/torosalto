(ns torosalto.agent)

(def instructions (slurp "instructions.md"))
(def rules (slurp "rules.md"))
(def notation (slurp "notation.md"))
(def prompt (str instructions "\n \n" rules "\n \n" notation))


