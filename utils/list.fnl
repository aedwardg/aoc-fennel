(fn any? [list func]
  (case list
    [h & t] (if (func h)
                true
                (any? t func))
    _ false))

{:any? any?}
