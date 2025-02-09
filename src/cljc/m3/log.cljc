(ns m3.log)

(defn info [& args]
  (apply println "INFO:" (mapv pr-str args)))

(defn warn [& args]
  (apply println "WARN:" (mapv pr-str args)))

(defn trace [& args]
  (apply println "TRACE:" (mapv pr-str args)))

(defn error [& args]
  (apply println "ERROR:" (mapv pr-str args)))
