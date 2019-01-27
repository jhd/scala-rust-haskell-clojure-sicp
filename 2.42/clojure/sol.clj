(defn queens [board-size]
  (defn queen-cols [k]
    (if (= k 0)
      (list empty-board)
      (filter (fn [positions] (safe? positions))
              (mapcat
               (fn [rest-of-queens]
                 (map (fn [new-queen]
                        (adjoin-position new-queen rest-of-queens))
                      (range 1 (+ board-size 1))))
               (queen-cols (- k 1))))))
  (queen-cols board-size))

(defn safe? [pos]
  (def queen-pos (first pos))
  (defn safe-iter [top bot remain]
    (defn f [] (first remain))
    (if (empty? remain) 
      true
      (cond (or (= (f) queen-pos)
                (= (f) top)
                (= (f) bot))
               false
             :else (safe-iter (- top 1) (+ bot 1) (rest remain)))))
  (safe-iter (- queen-pos 1) (+ queen-pos 1) (rest pos)))

(defn adjoin-position [new-queen rest-of-queens]
  (cons new-queen rest-of-queens))

(def empty-board '())

(defn -main [] 
    (println (queens 4))
    (println (queens 8)))
