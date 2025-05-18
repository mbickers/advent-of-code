(load "lib.ss")
(define (parse-steps input)
  (let
    ((parse-step (lambda (step) (list (car step) (string->number (cadr step))))))
    (map parse-step (groups 2 (words input)))))
(define (a input)
  (apply *
    (fold
      (lambda (state step)
        (map +
          state
          (cond
            ((equal? (car step) "forward") (list 0 (cadr step)))
            ((equal? (car step) "down") (list (cadr step) 0))
            ((equal? (car step) "up") (list (- (cadr step)) 0)))))
      (list 0 0)
      (parse-steps input))))
(define (b input)
  (let
    ((end-state (fold
      (lambda (state step)
        (map +
          state
          (cond
            ((equal? (car step) "forward") (list 0 (* (cadr step) (car state)) (cadr step)))
            ((equal? (car step) "down") (list (cadr step) 0 0))
            ((equal? (car step) "up") (list (- (cadr step)) 0 0)))))
      (list 0 0 0)
      (parse-steps input))))
    (apply * (cdr end-state))))