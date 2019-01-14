(defn same-parity [[x & xs]]
  (if (empty? xs)
    (print xs)
    (if (even? x)
      (cons x (filter even? xs))
      (cons x (filter odd? xs)))))

(defn -main [] 
  (println (same-parity (list 2 3 4 5 6 7)))
  (println (same-parity (list 1 2 3 4 5 6 7))))
