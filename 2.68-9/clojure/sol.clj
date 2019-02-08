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

(defn left-branch [tree] (first tree))

(defn right-branch [tree] (second tree))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else '() ))

(defn encode-symbol [sym tree]
  (cond (= tree '()) '()
        (= (symbol-leaf tree) sym) '()
        true (if (.contains (symbols (left-branch tree)) sym) 
          (cons 0 (encode-symbol sym (left-branch tree)))
          (cons 1 (encode-symbol sym (right-branch tree))))))


(defn decode [bits tree]
  (defn decode-1 [bits current-branch]
    (println bits " : " current-branch)
    (if (= bits '())
      '()
      (let [next-branch (choose-branch (first bits)
                                       current-branch)]
            (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (rest bits) tree))
              (decode-1 (rest bits) next-branch)))))
  (decode-1 bits tree))

(defn encode [message tree]
  (if (= message '())
    '()
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

(defn make-code-tree [left right]
  (list left
        right
        (list (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn adjoin-set [x pset]
  (cond (= pset `()) (list x)
        (< (weight x) (weight (first pset))) (cons x pset)
        :else (cons (first pset) (adjoin-set x (rest pset)))))

(defn make-leaf-set [pairs]
  (if (= pairs `())
    `()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) (second pair))
                  (make-leaf-set (rest pairs))))))

(defn successive-merge [pairs]
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

(defn -main []
  (let [tree (generate-huffman-tree (reverse `(("NA" 16) ("YIP" 9) ("SHA" 3) ("A" 2) ("GET" 2) ("JOB" 2) ("BOOM" 1) ("WAH" 1))))]
  (println tree)
  (println (encode (list "NA" "YIP" "SHA") tree))
  (println (decode (encode (list "NA" "YIP" "SHA") tree) tree))))
