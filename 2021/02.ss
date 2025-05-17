(load "lib.ss")
(define (a input)
  (letrec ((update (lambda (steps state)
    (if (null? steps)
      state
      (update
        (cdr steps)
        (let ((amount (string->number (cadar steps))))
          (map +
            state
            (cond
              ((equal? (caar steps) "forward") (list 0 amount))
              ((equal? (caar steps) "down") (list amount 0))
              ((equal? (caar steps) "up") (list (- amount) 0))))))))))
  (apply * (update (groups 2 (words input)) (list 0 0)))))
(define (b input)
  (letrec ((update (lambda (steps state)
    (if (null? steps)
      state
      (update
        (cdr steps)
        (let ((amount (string->number (cadar steps))))
          (map +
            state
            (cond
              ((equal? (caar steps) "forward") (list 0 (* amount (car state)) amount))
              ((equal? (caar steps) "down") (list amount 0 0))
              ((equal? (caar steps) "up") (list (- amount) 0 0))))))))))
  (apply * (cdr (update (groups 2 (words input)) (list 0 0 0))))))