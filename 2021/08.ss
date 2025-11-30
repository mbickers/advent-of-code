(load "lib.ss")
(define digits-to-wires
  (map
    (lambda (p) (list (car p) (symbol->char-list (cadr p))))
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
            (map
              (partial sort char<?)
              (split whitespace chunk))))
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
        (lambda (x) (length (cadr (assoc x digits-to-wires))))
        '(1 4 7 8))))
    (sum
      (apply
        append
        (map
          (compose cadr (partial map (compose length (in matching-lengths) bool->int)))
          pairs)))))
(define abcdefg (symbol->char-list 'abcdefg))
; give the list of lengths segment is a part of
(define (signature segment observed-output)
  (sort
    <
    (map
      length
      (filter
        (partial member segment)
        observed-output))))
(define wire-signatures
  (map
    (lambda (wire) (list wire (signature wire (map cadr digits-to-wires))))
    abcdefg))
(define (wire-for-signature signature)
  (pipe
    (find
      (lambda (p) (list=? = (cadr p) signature))
      wire-signatures)
    car))
(define (solve observed output)
  (let* (
    (segments-to-wires (map
      (lambda (segment)
        (list segment (wire-for-signature (signature segment observed))))
      abcdefg))
    (digits
      (map
        (lambda (digit-segments)
          (let* (
            (digit-wires
              (sort
                char<?
                (map
                  (lambda (segment) (cadr (assq segment segments-to-wires)))
                  digit-segments))))
            (car
              (find
                (lambda (p) (list=? char=? (cadr p) digit-wires))
                digits-to-wires))))
        output)))
    (fold
      (lambda (acc x) (+ (* 10 acc) x))
      0
      digits)))
(define (b input)
  (let* (
    (pairs (parse-input input)))
    (sum
      (map
        (lambda (pair) (solve (car pair) (cadr pair)))
        pairs))))