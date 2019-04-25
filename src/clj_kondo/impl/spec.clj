(ns clj-kondo.impl.spec
  {:no-doc true}
  (:require [clj-kondo.impl.utils :refer [parse-string]]
            [clj-kondo.impl.namespace :as namespace]
            [clojure.spec.alpha :as s]
            [rewrite-clj.node.protocols :as node]
            [clojure.set :as set]))

(declare analyze-spec)

(defn analyze-cat [ns cat-expr]
  (let [children (rest (:children cat-expr))]
    (loop [children children
           fixed-arity 0
           arities []]
      (if-let [k (first children)]
        (let [spec (second children)
              ?spec-arity (when (= :list (node/tag spec))
                            (analyze-spec ns spec))]
          (if-not ?spec-arity
            (recur (nnext children) (inc fixed-arity) arities)
            (recur (nnext children) fixed-arity (conj arities ?spec-arity))))
        (do
          (println "arities" arities)
          (reduce (fn [acc arity]
                     (println "acc" acc "arity" arity)
                     (if-let [fa (:fixed-arities arity)]
                       {:fixed-arity (set/union fa (:fixed-arity acc))}
                       (do
                         (println "REDUCED" arity (max (:fixed-arity acc)))
                         (reduced (merge acc
                                         (update arity :var-args-min-arity
                                                 + (apply max (:fixed-arity acc))))))))
                   {:fixed-arity #{fixed-arity}}
                   arities))))))

(defn analyze-star [star-expr]
  {:var-args-min-arity 0})

(defn analyze-plus [plus-expr]
  {:var-args-min-arity 1})

(defn analyze-qm [qm-expr]
  {:fixed-arities #{0 1}})

(defn analyze-spec [ns spec]
  (let [resolved-call (namespace/resolve-name ns (:value (first (:children spec))))
        arity (case [(:ns resolved-call) (:name resolved-call)]
                [clojure.spec.alpha cat]
                (analyze-cat ns spec)
                [clojure.spec.alpha *]
                (analyze-star spec)
                [clojure.spec.alpha ?]
                (analyze-qm spec)
                nil)]
    arity))

(defn analyze-fdef-arities [ns fdef-expr]
  (let [children (rest (:children fdef-expr))
        sym (:value (first children))
        resolved-sym (namespace/resolve-name ns sym)
        ;; TODO: look at actual keys ;-)
        args-spec (nth children 2)
        arities (analyze-spec ns args-spec)]
    {(:ns resolved-sym) {(:name resolved-sym) arities}}))

(comment
  (def my-ns (namespace/analyze-ns-decl
              :clj
              (parse-string "(ns foo (:require [clojure.spec.alpha :as s] [foo]))")))
  (def spec-expr (parse-string "(s/fdef clojure.core/assoc :args (s/cat :m map? :k keyword? :v any?))"))
  (analyze-fdef-arities my-ns spec-expr)

  (def spec-expr (parse-string "(s/fdef foo :args (s/* int?))"))
  (analyze-fdef-arities my-ns spec-expr)

  (def spec-expr (parse-string "(s/cat :x int? :z (s/? string?) :y (s/* int?))"))
  (analyze-spec my-ns spec-expr) ;; TODO: not right yet

  (def maps-spec-expr (parse-string "(s/def ::maps (s/* map?))"))
  (def spec-expr (parse-string
                  "(s/fdef my-fn :args (s/cat :m map? :maps ::maps))"))
  (analyze-fdef-arities my-ns spec-expr)
  )
