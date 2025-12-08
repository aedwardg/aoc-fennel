(local grid (icollect [line (io.lines "year2025/day07/input.txt")]
              (icollect [char (string.gmatch line ".")] char)))

(local [start-y start-x] [1 (// (length grid) 2)])

; part 1
(local seen {})

(fn traverse [y x total]
  (let [next-y (+ y 1)
        key (.. next-y "," x)]
    (if (= (. grid next-y) nil) total
        (case (. grid next-y x)
          "." (traverse next-y x total)
          (where "^" (. seen key)) total
          "^" (do
                (set (. seen key) true)
                (+ (traverse next-y (- x 1) (+ total 1))
                   (traverse next-y (+ x 1) 0)))))))

(traverse start-y start-x 0)

; part 2

(local seen-nodes {})

(fn create-node [y x] {:val (.. y "," x)})

(local start-node {:val (table.concat [start-y start-x] ",")})

(fn build-tree [node y x]
  (let [next-y (+ y 1)
        key (.. next-y "," x)]
    (if (= (. grid next-y) nil) node
        (case (. grid next-y x)
          "." (build-tree node next-y x)
          (where "^" (. seen-nodes key)) (do
                                           (set (. node :left)
                                                (. seen-nodes key :left))
                                           (set (. node :right)
                                                (. seen-nodes key :right)))
          "^" (let [left (create-node next-y (- x 1))
                    right (create-node next-y (+ x 1))]
                (set (. seen-nodes key) {:left left :right right})
                (set (. node :left) left)
                (set (. node :right) right)
                (build-tree node.left next-y (- x 1))
                (build-tree node.right next-y (+ x 1)))))))

; build the tree, now available via start-node
(build-tree start-node start-y start-x)

(fn is-leaf? [node] (not (or node.left node.right)))

(local memo {})

(fn count-paths [node]
  (case node
    nil 0
    (where node (. memo node.val)) (. memo node.val)
    (where node (is-leaf? node)) (do
                                   (set (. memo node.val) 1)
                                   1)
    _ (let [count (+ (count-paths node.left) (count-paths node.right))]
        (set (. memo node.val) count)
        count)))

(count-paths start-node)
