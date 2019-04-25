(ns clj-kondo.impl.spec
  {:no-doc true}
  (:require [clj-kondo.impl.utils :refer [parse-string]]
            [clj-kondo.impl.namespace :as namespace]
            [clojure.spec.alpha :as s]))

(defn analyze-cat [cat-expr]
  (let [children (rest (:children cat-expr))]
    (loop [children children
           arity 0]
      (if-let [c (first children)]
        (recur (nnext children) (inc arity))
        arity))))

(defn analyze-start [star-expr]
  100)

(defn analyze-fdef-arities [ns fdef-expr]
  (let [children (rest (:children fdef-expr))
        sym (:value (first children))
        resolved-sym (namespace/resolve-name ns sym)
        ;; TODO: look at actual keys ;-)
        args-spec (nth children 2)
        resolved-call (namespace/resolve-name ns (:value (first (:children args-spec))))
        arity (case [(:ns resolved-call) (:name resolved-call)]
                [clojure.spec.alpha cat]
                (analyze-cat args-spec))]
    {(:ns resolved-sym) {(:name resolved-sym) arity}}))

(comment
  (def my-ns (namespace/analyze-ns-decl
              :clj
              (parse-string "(ns foo (:require [clojure.spec.alpha :as s] [foo]))")))
  (def spec-expr (parse-string "(s/fdef clojure.core/assoc :args (s/cat :m map? :k keyword? :v any?))"))
  (analyze-fdef-arities my-ns spec-expr)

  (def maps-spec-expr (parse-string "(s/def ::maps (s/* map?))"))
  (def spec-expr (parse-string
                  "(s/fdef my-fn :args (s/cat :m map? :maps ::maps))"))
  (analyze-fdef-arities my-ns spec-expr)
  )



