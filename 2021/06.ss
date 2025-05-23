(load "lib.ss")
(define (parse input)
  (map
    (partial char-list->number 10)
    (split
      (partial eq? #\,)
      (filter (lambda (c) (not (whitespace c))) (string->list input)))))
(define (a input)
  (define (step state)
    (fold
      (lambda (state c)
        (let ((state (alist-update-default (car c) 0 (lambda (x) (- x (cadr c))) state)))
          (let ((state (alist-update-default (if (eq? (car c) 0) 6 (- (car c) 1)) 0 (lambda (x) (+ x (cadr c))) state)))
            (if 
              (eq? (car c) 0)
              (alist-update-default 8 0 (lambda (x) (+ x (cadr c))) state)
              state))))
      state
      state))
  (define (simulate steps state)
    (if (eq? steps 0)
      state
      (simulate (-  steps 1) (step state))))
  (apply + (map cadr (simulate 80 (freqs (parse input))))))