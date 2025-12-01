(load "lib.ss")
(define (parse-input input)
  (pipe
    (lines input)
    (partial map string->list)
    (partial map (partial map char->number))))
(define offsets '((0 1) (0 -1) (1 0) (-1 0)))
(define (solve f input)
  (letrec* (
    (parsed (parse-input input))
    (rows (length parsed))
    (columns (length (car parsed)))
    (fill-oob (lambda (default x) (if (eq? x 'oob) default x)))
    (height (lambda (pos) (cond
      ((any (partial > 0) pos) 'oob)
      ((>= (car pos) rows) 'oob)
      ((>= (cadr pos) columns) 'oob)
      (else (list-ref (list-ref parsed (car pos)) (cadr pos))))))
    (new-predecessors (lambda (p visited)
      (fold
        (lambda (visited offset)
          (let* (
            (new-point (map + p offset)))
            (if
              (or
                (eq? (height new-point) 'oob)
                (= (height new-point) 9)
                (< (height new-point) (height p))
                (find (partial list=? = new-point) visited))
              visited
              (new-predecessors new-point visited))))
        (cons p visited)
        offsets))))
    (pipe
      (product (iota rows) (iota columns))
      (partial filter (lambda (pos)
        (all
          (lambda (offset) (<
            (height pos)
            (fill-oob 10 (height (map + pos offset)))))
          offsets)))
      (partial f height))))
(define a
  (partial
    solve
    (lambda (height basins)
      (pipe
        basins
        (partial map (compose height (partial + 1)))
        sum))))
(define b
  (partial
    solve
    (lambda (height basins)
      (letrec (
        (new-predecessors (lambda (p visited)
          (fold
            (lambda (visited offset)
              (let* (
                (new-point (map + p offset)))
                (if
                  (or
                    (eq? (height new-point) 'oob)
                    (= (height new-point) 9)
                    (< (height new-point) (height p))
                    (find (partial list=? = new-point) visited))
                  visited
                  (new-predecessors new-point visited))))
            (cons p visited)
            offsets))))
        (pipe
          basins
          (partial map (lambda (low-point) (new-predecessors low-point '())))
          (partial map length)
          (partial sort >)
          (partial split-on 3)
          car
          (partial apply *))))))