(local grid (icollect [line (io.lines "year2025/day07/input.txt")]
              (icollect [char (string.gmatch line ".")] char)))

(local [start-y start-x] [1 (// (length grid) 2)])

; part 1
(local seen {})

(fn traverse [y x total]
  (let [next-y (+ y 1)
        key (table.concat [(+ y 1) x] ",")]
    (if (= (. grid next-y) nil) total
        (case (. grid next-y x)
          "." (traverse next-y x total)
          (where "^" (. seen key)) total
          "^" (do
                (set (. seen key) true)
                (+ (traverse next-y (- x 1) (+ total 1))
                   (traverse next-y (+ x 1) 0)))))))

(traverse start-y start-x 0)
