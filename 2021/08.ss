(load "lib.ss")
(define letters
  (map
    (lambda (p) (list (car p) (string->list (symbol->string (cadr p)))))
    '((0 abcefg)
      (1 cf)
      (2 acdeg)
      (3 acdfg)
      (4 bcdf)
      (5 abdfg)
      (6 abdefg)
      (7 acf)
      (8 abcdefg)
      (9 abcdfg))))
(define (parse-input input)
  (let* (
    (unpaired
      (split
        (lambda (c) (or (equal? c #\newline) (equal? c #\|)))
        (string->list input)))
    (unpaired-words
      (map
        (lambda (chunk)
          (filter
            (lambda (s) (> (length s) 0))
            (split whitespace chunk)))
        unpaired))
    (pairs
      (groups
        2
        (filter
          (lambda (l) (> (length l) 0))
          unpaired-words))))
    pairs))
(define (a input) 
  (let* (
    (pairs (parse-input input))
    (matching-lengths
      (map
        (lambda (x) (length (cadr (assoc x letters))))
        '(1 4 7 8))))
    (sum
      (apply
        append
        (map
          (compose cadr (partial map (compose length (in matching-lengths) bool->int)))
          pairs)))))
(define (b input)
  (let* (
    (pairs (parse-input input))
    (matching-lengths
      (map
        (lambda (x) (length (cadr (assoc x letters))))
        '(1 4 7 8))))
    (sum
      (apply
        append
        (map
          (compose cadr (partial map (compose length (in matching-lengths) bool->int)))
          pairs)))))