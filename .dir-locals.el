;; ((clojure-mode . ((cider-clojure-cli-global-options . "-A:clj-xchart:test.check"))))
((clojure-mode
  (cider-preferred-build-tool
   . "clojure-cli")
  (cider-clojure-cli-parameters
   . "-A:dev:kaocha -m nrepl.cmdline --middleware '%s'")))
