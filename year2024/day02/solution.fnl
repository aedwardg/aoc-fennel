(local lines (icollect [line (io.lines "year2024/day02/input.txt")] line))

(local splits
       (icollect [_ line (ipairs lines)]
         (icollect [sp (string.gmatch line "[^ ]+")] (tonumber sp))))

; part 1
(fn is-safe? [list curr i dir]
  (case [list curr i dir]
    (where [list _ i _] (> i (length list))) true
    (where [list curr i _] (= curr (. list i))) false
    (where [list curr i :asc] (> curr (. list i))) false
    (where [list curr i :desc] (< curr (. list i))) false
    (where [list curr i _] (< 3 (math.abs (- curr (. list i))))) false
    [list curr i ?dir]
    (let [direction (or ?dir (if (< curr (. list i)) :asc :desc))]
      (is-safe? list (. list i) (+ 1 i) direction))))

(accumulate [total 0 _ list (ipairs splits)]
  (if (is-safe? list (. list 1) 2 nil) (+ total 1) total))

; part 2
(fn group-without-index [list i]
  (let [new-list (table.move list 1 (length list) 1 {})]
    (table.remove new-list i)
    new-list))

(fn create-groups [list]
  (let [groups (icollect [i _ (ipairs list)]
                 (group-without-index list i))]
    (table.insert groups 1 list)
    groups))

(fn any? [list func n]
  (let [i (or n 1)]
    (if (> i (length list)) false
        (func list i) true
        (any? list func (+ 1 i)))))

(accumulate [total 0 _ list (ipairs splits)]
  (let [groups (create-groups list)]
    (if (any? groups (fn [list i] (is-safe? (. list i) (. list i 1) 2 nil)))
        (+ total 1)
        total)))
