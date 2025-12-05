(local list (require :utils.list))

(local lines (icollect [line (io.lines "year2025/day05/input.txt")]
               (icollect [char (string.gmatch line "%w+")] char)))

(local ranges (icollect [_ line (ipairs lines)]
                (case line
                  [start end] [(tonumber start) (tonumber end)])))

(local ids (icollect [_ line (ipairs lines)]
             (if (= 1 (length line)) (tonumber (. line 1)))))

; part 1
(fn is-fresh? [id]
  (list.any? ranges (fn [[start end]] (and (>= id start) (>= end id)))))

(accumulate [total 0 _ id (ipairs ids)]
  (if (is-fresh? id)
      (+ total 1)
      total))

; part 2
(local range-map (let [map {}]
                   (each [_ [start end] (ipairs ranges)]
                     (case (. map start)
                       nil (set (. map start) end)
                       (where a (> end a)) (set (. map start) end)))
                   map))

(local all-possible (accumulate [total 0 start end (pairs range-map)]
                      (+ total (- end start) 1)))

(local num-overlap (let [keys (icollect [k _ (pairs range-map)] k)]
                     (table.sort keys)
                     (faccumulate [overlap 0 i 1 (- (length keys) 1)]
                       (let [s1 (. keys i)
                             e1 (. range-map s1)
                             s2 (. keys (+ i 1))
                             e2 (. range-map s2)]
                         (if (= e1 s2) (+ overlap 1) ; 1 point overlapping
                             (= e1 e2) (+ overlap (- e2 s2) 1) ;same end
                             ; partial overlap
                             (and (> e1 s2) (> e2 e1)) (+ overlap (- e1 s2) 1)
                             (> e1 e2) ; full overlap
                             (do
                               (set (. range-map s2) e1)
                               (+ overlap (- e2 s2) 1))
                             overlap ; no overlap
                             )))))

(- all-possible num-overlap)

; [3 4 5]
;     [5 5]
;        [10 11 12 13 14]
;           [11 12 13]
;              [12 13 14 15 16 17 18]
;                 [13 14 15]
;                          [16 17 18 19 20]
;                             [17 18 19 20]
;                                [18 19 20]
;                                   [19 20]
