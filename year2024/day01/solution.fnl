(local lines (icollect [line (io.lines "year2024/day01/input.txt")]
               line))

(fn separate-splits [splits left right]
  (each [_ split (ipairs splits)]
    (table.insert left (. split 1))
    (table.insert right (. split 2)))
  (table.sort left)
  (table.sort right)
  [left right])

; part 1
(let [splits (icollect [_ line (ipairs lines)]
               (icollect [sp (string.gmatch line "[^ ]+")] sp))]
  (let [[left right] (separate-splits splits {} {})]
    (accumulate [acc 0 i n (ipairs left)]
      (+ acc (math.abs (- n (. right i)))))))

; part 2

(let [splits (icollect [_ line (ipairs lines)]
               (icollect [sp (string.gmatch line "[^ ]+")] sp))]
  (let [[left right] (separate-splits splits {} {})]
    (accumulate [acc 0 _ n (ipairs left)]
      (let [freq (accumulate [count 0 _ m (ipairs right)]
                   (if (= m n) (+ count 1) count))]
        (+ acc (* n freq))))))
