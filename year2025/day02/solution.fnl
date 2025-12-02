(local [input] (icollect [line (io.lines "year2025/day02/input.txt")] line))

(local ranges
       (icollect [sp (string.gmatch input "[^,]+")]
         (icollect [num (string.gmatch sp "[^-]+")] (tonumber num))))

; part 1
(fn invalid? [num]
  (case (tostring num)
    (where num-str (-> num-str
                       (length)
                       (% 2)
                       (not= 0))) false
    num-str (let [mid (// (length num-str) 2)
                  left (string.sub num-str 1 mid)
                  right (string.sub num-str (+ mid 1) -1)]
              (= left right))))

(accumulate [sum 0 _ [start end] (ipairs ranges)]
  (+ sum (faccumulate [acc 0 id start end] (if (invalid? id) (+ acc id) acc))))

; part 2
(fn repeat [str i]
  (-> str
      (string.sub 1 i)
      (string.rep (// (length str) i))
      (tonumber)))

; TODO: extract into reusable utitlity
(fn any? [list func]
  (case list
    [h & t] (if (func h)
                true
                (any? t func))
    _ false))

(fn invalid2? [num]
  (let [num-str (tostring num)
        mid (// (length num-str) 2)
        combos (fcollect [i 1 mid] (repeat num-str i))]
    (any? combos (fn [n] (= n num)))))

(accumulate [sum 0 _ [start end] (ipairs ranges)]
  (+ sum (faccumulate [acc 0 id start end] (if (invalid2? id) (+ acc id) acc))))
