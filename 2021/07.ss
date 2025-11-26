(load "lib.ss")
(define (parse-input input)
  (map
    (compose list->string string->number)
    (split
      (lambda (c) (equal? c #\,))
      (string->list input))))
(define (solver choose cost)
  (lambda (input)
    (let* (
      (parsed (parse-input input))
      (possible-positions (choose parsed)))
      (max-by
        -
        (map
          (compose
            (lambda (position)
              (map
                (lambda (sub-pos) (cost position sub-pos))
                parsed))
            sum)
          possible-positions)))))
(define a
  (solver
    (compose median list)
    (lambda (pos sub-pos) (abs (- pos sub-pos)))))
(define b
  (solver
    (lambda (positions) (list (floor (mean positions)) (ceiling (mean positions))))
    (lambda (pos sub-pos)
      (let ((n (abs (- pos sub-pos))))
      (* n (+ n 1) 1/2)))))