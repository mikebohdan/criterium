;; ((clojure-mode . ((cider-clojure-cli-global-options . "-A:clj-xchart:test.check"))))
((clojure-mode
  (cider-preferred-build-tool
   . "clojure-cli")
  (cider-clojure-cli-global-options
   . "-J-Dignore.symbol.file -A:dev:kaocha")
  (eval .
        (define-clojure-indent
          ;; Please keep this list sorted
          (arg-gen/measured 2))))
 (cider-mode
  (cider-preferred-build-tool
   . "clojure-cli")
  (cider-clojure-cli-global-options
   . "-A:dev:kaocha")))  ;;  -m nrepl.cmdline --middleware '%s'
