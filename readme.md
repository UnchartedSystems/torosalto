# Toro Salto
*Torus Hop* ◦ An abstract strategy game for two players.

## This Repository

The purpose of this project is to create a correspondence web app, a cli app, an AI harness, and an AI benchmark for ToroSalto.

## OpenAI

Sign in with your ChatGPT account from a local REPL:

```clojure
(require '[torosalto.api.openai :as openai])
(openai/login!)
(openai/models!)
(openai/ask! {:model "<an ID returned by models!>"
              :question "What is the capital of Algeria?"})
```

`login!` returns nonsecret account information. Calls use the saved account and
automatically renew credentials near access-token expiry, including after restart.
Existing access-only credentials need one more browser sign-in to enable renewal.
Use `(openai/login! {:force? true})` to explicitly sign in again.

Credentials live under `:openai` in `secrets.edn` in the working directory. Access
and rotating refresh tokens are saved atomically with owner-only permissions;
other entries are preserved. Processes sharing this file coordinate renewal using
`secrets.edn.lock`. Keep that sidecar in place while processes are running.
Inference requests can run concurrently within one JVM or across processes.

API calls report when browser sign-in is needed; they never launch a browser.
Temporary refresh failures preserve credentials so you can retry later. Each
credential file holds one OpenAI account registration.
