(load "lib.ss")
(define parse-input (compose lines (partial map string->list)))
(define pairs (map string->list '("()" "[]" "{}" "<>")))
(define (process stack line)
  (if
    (null? line)
    `(incomplete ,stack)
    (let (
      (pair-this-closes (find (lambda (p) (eq? (cadr p) (car line))) pairs)))
      (if
        pair-this-closes
        (if
          (and (not (null? stack)) (eq? (car pair-this-closes) (car stack)))
          (process (cdr stack) (cdr line))
          `(corrupted ,(cadr pair-this-closes)))
        (process (cons (car line) stack) (cdr line))))))
(define (a input)
  (pipe
    input
    parse-input
    (partial map (partial process '()))
    (partial filter (lambda (x) (eq? (car x) 'corrupted)))
    (partial map (lambda (x) (cadr (assq (cadr x) (zip (map cadr pairs) '(3 57 1197 25137))))))
    sum))
(define (b input)
  (pipe
    input
    parse-input
    (partial map (partial process '()))
    (partial filter (lambda (x) (eq? (car x) 'incomplete)))
    (partial map (lambda (x)
      (fold
        (lambda (acc p) (+
          (* acc 5)
          (cadr (assoc p (zip (map car pairs) '(1 2 3 4))))))
        0
        ; (lambda (acc p) (string-append acc (list->string (list p))))
        ; ""
        (cadr x))))
    (partial sort <)
    (lambda (l) (list-ref l (floor (/ (length l) 2))))))