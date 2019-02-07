(defn leaf? [x]
  (= (first x) `leaf))

(defn symbol-leaf [x] (second x))

(defn weight-leaf [x] (nth x 2))

(defn make-leaf [sym weight]
  (list `leaf sym weight))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn symbols [tree]
  (if (leaf? tree)
    (symbol-leaf tree)
    (nth tree 2)))

(defn make-code-tree [left right]
  (list left
        right
        (list (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn adjoin-set [x pset]
  ;(println "as" x " : " pset)
  (cond (= pset `()) (list x)
        (< (weight x) (weight (first pset))) (cons x pset)
        :else (cons (first pset) (adjoin-set x (rest pset)))))

(defn make-leaf-set [pairs]
  ;(println pairs)
  (if (= pairs `())
    `()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) (second pair))
                  (make-leaf-set (rest pairs))))))

(defn successive-merge [pairs]
  ;(println "SM" pairs)
  (cond (= pairs `()) `()
        (= (rest pairs) `()) (first pairs)
        :else (successive-merge
                (adjoin-set (make-code-tree
                              (first pairs)
                              (second pairs))
                            (nthrest pairs 2)))))

(defn generate-huffman-tree [freq-list]
  (println freq-list)
  (successive-merge (make-leaf-set freq-list)))


(defn encode [])

(defn decode [])

(defn -main []
  (let [tree (generate-huffman-tree (reverse `(("NA" 16) ("YIP" 9) ("SHA" 3) ("A" 2) ("GET" 2) ("JOB" 2) ("BOOM" 1) ("WAH" 1))))]
  (println tree)))
