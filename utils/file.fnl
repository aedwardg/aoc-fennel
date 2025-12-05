(fn read-file [path]
  (with-open [file (io.open path)]
    (if file
        (file:read "*all")
        (error (.. "Could not read file " path)))))

{:read-file read-file}
