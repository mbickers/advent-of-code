(load "lib.ss")
(define (a input)
  (let* ((columns
        (apply zip (split (partial equal? #\newline) (string->list input))))
      (gamma (char-list->number 2 (map (partial by-freq 'most) columns)))
      (beta (char-list->number 2 (map (partial by-freq 'least) columns))))
    (* gamma beta)))
(define (b input)
  (let* ((rows
        (split (partial equal? #\newline) (string->list input)))
      (greatest-freq-high-bias (lambda (o) (+ (cadr o) (/ (char->number (car o)) 10))))
      (filter-by-digit-until-one (lambda (cmp)
        (until-one
            (lambda (round rows)
              (let ((digit
                  (car
                    (max-by
                      cmp
                      (freqs (map (lambda (row) (list-ref row round)) rows))))))
                (filter
                  (lambda (row) (equal? (list-ref row round) digit))
                  rows)))
            rows)))
      (oxygen-generator (char-list->number 2 (filter-by-digit-until-one greatest-freq-high-bias)))
      (co2-scrubber (char-list->number 2 (filter-by-digit-until-one (compose greatest-freq-high-bias -)))))
    (* oxygen-generator co2-scrubber)))