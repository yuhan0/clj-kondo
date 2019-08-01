(ns clj-kondo.tools.underscored-namespaces
  (:require [clj-kondo.core :as clj-kondo]
            [clojure.string :as str]))

(defn -main [& paths]
  (let [analysis (:analysis (clj-kondo/run! {:lint paths
                                             :config {:output {:analysis true}}}))
        {:keys [:namespace-definitions :namespace-usages]} analysis
        names (set (map :name namespace-definitions))
        munged-names (set (map munge names))]
    (doseq [{:keys [:to :filename :row :col]} namespace-usages]
      (let [munged (munge to)
            munged? (not= to munged)]
        (when (and munged? (contains? names munged))
          (println (str filename ":" row ":" col ": warning:")
                   "namespace" munged "is spelled with underscores but required as" to))
        (when (and (not munged?)
                   (contains? munged-names to))
          (println (str filename ":" row ":" col ": warning:")
                   "namespace" (str/replace to "_" "-") "is spelled with hyphens but required as" to))))))
