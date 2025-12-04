(local lines (icollect [line (io.lines "year2025/day04/input.txt")]
               (icollect [char (string.gmatch line "(.)")] char)))

(fn get-adj [grid [x y]]
  (let [coords [[(- x 1) (- y 1)]
                [x (- y 1)]
                [(+ x 1) (- y 1)]
                [(- x 1) y]
                [(+ x 1) y]
                [(- x 1) (+ y 1)]
                [x (+ y 1)]
                [(+ x 1) (+ y 1)]]]
    (accumulate [acc {} _ [x y] (ipairs coords)]
      (case (?. grid x y)
        "@" (do
              (table.insert acc [x y])
              acc)
        _ acc))))

(fn removable [grid]
  (accumulate [acc [] y line (ipairs grid)]
    (accumulate [removed acc x _ (ipairs line)]
      (if (= (. grid x y) "@")
          (case (get-adj grid [x y])
            (where adj (> 4 (length adj))) (do
                                             (table.insert removed [x y])
                                             removed)
            _ removed)
          removed))))

; part 1
(length (removable lines))

; part 2
(fn remove-rolls [grid rolls]
  (each [_ [x y] (ipairs rolls)] (set (. grid x y) ".")))

(fn remove-all [grid total]
  (let [rolls (removable grid)]
    (case (length rolls)
      0 total
      r (do
          (remove-rolls grid rolls)
          (remove-all grid (+ total r))))))

(remove-all lines 0)
