;; ((clojure-mode . ((cider-clojure-cli-global-options . "-A:clj-xchart:test.check"))))
((clojure-mode
  (cider-preferred-build-tool
   . "clojure-cli")
  (cider-clojure-cli-parameters
   . "-A:dev:kaocha -m nrepl.cmdline --middleware '%s'")
  (eval .
        (define-clojure-indent
          ;; Please keep this list sorted
          (arg-gen/measured 2)))
