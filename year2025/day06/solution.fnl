(local lines (icollect [line (io.lines "year2025/day06/input.txt")] line))
(local operators (icollect [char (string.gmatch (table.remove lines) "[%+%*]")]
                   char))

; part 1
(local numbers
       (icollect [_ line (ipairs lines)]
         (icollect [char (string.gmatch line "%d+")] (tonumber char))))

(faccumulate [sum 0 col 1 (length operators)]
  (+ (let [[start fun] (case (. operators col)
                         "+" [0 #(+ $1 $2)]
                         "*" [1 #(* $1 $2)])]
       (faccumulate [acc start row 1 (length numbers)]
         (fun acc (. numbers row col)))) sum))

; part 2
(local num-chars
       (icollect [_ line (ipairs lines)]
         (icollect [char (string.gmatch line ".")] char)))

(local reversed-nums (fcollect [col (length (. num-chars 1)) 1 -1]
                       (or (tonumber (faccumulate [acc "" row 1 (length num-chars)]
                                       (.. acc (. num-chars row col))))
                           "|")))

(local reversed-ops (fcollect [i (length operators) 1 -1] (. operators i)))

(local groups (accumulate [acc [[]] _ num (ipairs reversed-nums)]
                (case num
                  "|" (do
                        (table.insert acc [])
                        acc)
                  _ (do
                      (table.insert (. acc (length acc)) num)
                      acc))))

(faccumulate [sum 0 col 1 (length reversed-ops)]
  (+ (let [[start fun] (case (. reversed-ops col)
                         "+" [0 #(+ $1 $2)]
                         "*" [1 #(* $1 $2)])]
       (accumulate [acc start _ num (ipairs (. groups col))]
         (fun acc num))) sum))
