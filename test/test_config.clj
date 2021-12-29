(ns test-config)

(defmacro read-config [file]
  (read-string (slurp file)))



