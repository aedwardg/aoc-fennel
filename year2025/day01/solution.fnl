(local lines (icollect [line (io.lines "year2025/day01/input.txt")]
               line))

(local rotations (icollect [_ line (ipairs lines)]
                   (let [(letter number) (string.match line "(%a)(%d+)")]
                     [letter (tonumber number)])))

(fn turn [curr rotation]
  (case rotation
    ["L" n] (% (- curr n) 100)
    ["R" n] (% (+ curr n) 100)))

; part 1
(fn count-zeros [rots i curr acc]
  (if (> i (length rots))
      acc
      (let [new-curr (turn curr (. rots i))
            new-acc (if (= new-curr 0) (+ 1 acc) acc)]
        (count-zeros rots (+ 1 i) new-curr new-acc))))

(let [num-zeros (count-zeros rotations 1 50 0)] num-zeros)

; part 2

(fn add-all-zeros [acc curr rotation]
  (let [dist-to-zero (case rotation ["L" _] curr ["R" _] (- 100 curr))]
    (case rotation
      (where [_ n] (= n dist-to-zero)) (+ acc 1)
      (where [_ n] (< n dist-to-zero)) acc
      (where [_ n] (> n dist-to-zero)) (let [extra-zeros (// (- n dist-to-zero)
                                                             100)]
                                         (if (= 0 dist-to-zero)
                                             (+ acc extra-zeros)
                                             (+ acc 1 extra-zeros))))))

(fn count-all-zeros [rots i curr acc]
  (if (> i (length rots))
      acc
      (let [new-curr (turn curr (. rots i))
            new-acc (add-all-zeros acc curr (. rots i))]
        (count-all-zeros rots (+ 1 i) new-curr new-acc))))

(let [num-zeros (count-all-zeros rotations 1 50 0)] num-zeros)
