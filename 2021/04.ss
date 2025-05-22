(load "lib.ss")
(define (board grid marks)
  (let*
    ((show (lambda () (groups 5 (map (lambda (g m) (if m (list g) g)) grid marks))))
    (update (lambda (n) (board
      grid
      (map (lambda (g m) (if (eq? g n) #t m)) grid marks))))
    (finished (lambda () (or
      (any (partial all id) (groups 5 marks))
      (any (partial all id) (apply zip (groups 5 marks))))))
    (score (lambda ()
      (apply
        +
        (map
          (lambda (g m) (if m 0 g))
          grid
          marks)))))
    (lambda (message)
      (cond
        ((eq? 'show message) show)
        ((eq? 'update message) update)
        ((eq? 'finished message) finished)
        ((eq? 'score message) score)
        (else (raise "no message"))))))
(define (parse input)
  (let* ((order-and-boards (words input))
       (order (map (partial char-list->number 10) (split (partial eq? #\,) (string->list (car order-and-boards)))))
       (boards (map (lambda (b) (board b (repeat 25 #f))) (groups 25 (map string->number (cdr order-and-boards))))))
    (values order boards)))
(define (a input)
  (let-values (((draws boards) (parse input)))
    (letrec
      ((run (lambda (draws boards)
        (let ((boards (map (lambda (board) ((board 'update) (car draws))) boards)))
          (let ((finished-board-sublist (first-sublist
              (lambda (b) ((b 'finished)))
              boards)))
            (if (null? finished-board-sublist)
              (run (cdr draws) boards)
              (*
                (car draws)
                (((car finished-board-sublist) 'score)))))))))
      (run draws boards))))
(define (b input)
  (let-values (((draws boards) (parse input)))
    (letrec
      ((run (lambda (draws boards)
        (let ((boards (map (lambda (board) ((board 'update) (car draws))) boards)))
          (let ((unfinished-boards (filter
              (lambda (b) (not ((b 'finished))))
              boards)))
            (if (null? unfinished-boards)
              (*
                (car draws)
                (((car boards) 'score)))
              (run (cdr draws) unfinished-boards)))))))
      (run draws boards))))