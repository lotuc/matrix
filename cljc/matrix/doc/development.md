# Matrix development

## Running tests

Running Clojure tests with Cognitect's
[test-runner](https://github.com/cognitect-labs/test-runner)

```sh
clojure -M:test
```

Running ClojureScript tests with
[cljs-test-runner](https://github.com/Olical/cljs-test-runner)

```sh
clojure -M:cljs-test
# test with Closure Compiler advanced optimisation
clojure -M:cljs-test -c ./advanced-compilation.edn
```
