(local banks (icollect [line (io.lines "year2025/day03/input.txt")]
               (icollect [char (string.gmatch line "(.)")] char)))

; part 1
(fn get-tens [bank]
  (accumulate [[high pos] ["0" 0] i char (ipairs bank)]
    (if (and (> char high) (> (length bank) i)) [char i] [high pos])))

(fn get-ones [bank start]
  (accumulate [high "0" i char (ipairs bank)]
    (if (and (> char high) (> i start)) char high)))

(accumulate [sum "0" _ bank (ipairs banks)]
  (let [[tens pos] (get-tens bank)
        ones (get-ones bank pos)]
    (+ sum (.. tens ones))))

; part 2
(fn batteries [list start high rem acc]
  (var new-start start)
  (var new-high high)
  (case rem
    0 acc
    _ (do
        (for [i new-start (- (+ (length list) 1) rem)]
          (when (> (. list i) new-high)
            (set new-high (. list i))
            (set new-start (+ i 1))))
        (batteries list new-start "0" (- rem 1) (.. acc new-high)))))

(accumulate [sum "0" _ bank (ipairs banks)]
  (+ sum (batteries bank 1 "0" 12 "")))
