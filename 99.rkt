#lang racket


; 1
(define (my-last xs)
  (cond ((empty? xs) (error "dio"))
        ((empty? (cdr xs)) xs)
        (else (my-last (cdr xs)))))

(define (my-last1 xs)
  (if (empty? xs) (error "diooooo")
    (let loop ((xs xs) (ys (cdr xs)))
      (if (empty? ys) xs
        (loop ys (cdr ys))))))

; 2
(define (my-but-last xs)
  (if (or (empty? xs) (empty? (cdr xs)))
    (error "list too short")
    (let loop ((xs xs) (ys (cdr xs)) (zs (cddr xs)))
      (if (empty? zs) xs
        (loop ys zs (cdr zs))))))

; 3
(define (element-at xs i)
  (cond ((empty? xs) (error "index out of bounds"))
        ((eq? i 1) (car xs))
        (else (element-at (cdr xs) (- i 1)))))

; 4
(define (my-length xs)
  (let loop ((xs xs) (len 0))
    (if (empty? xs) len
      (loop (cdr xs) (+ len 1)))))

; 5
(define (my-reverse xs)
  (let loop ((xs xs) (ys '()))
    (if (empty? xs) ys
      (loop (cdr xs) (cons (car xs) ys)))))

; 6
(define (palindrome? xs)
  (equal? xs (reverse xs)))

; 7
(define (my-flatten xs)
  (define (aux xs res)
    (if (empty? xs) res
      (let ((x (car xs)))
        (aux (cdr xs)
             (if (list? x) (aux x res) (cons x res))))))
  (reverse (aux xs '())))

; 8
(define (pack1 xs)
  (let loop ((c 0) (x null) (xs xs) (pack '()) (res '()))
    (cond ((empty? xs) (reverse res))
          ((equal? c x) (loop x (car xs) (cdr xs) (cons c pack) res))
          (else (loop x (car xs) (cdr xs) '() (cons pack res))))))

; 9
(define (pack xs)
  (if (empty? xs) xs
    (let ((x (car xs)) (xs (cdr xs)))
      (let loop ((x x) (xs xs) (pack (list x)) (res '()))
        (if (empty? xs) (reverse (cons pack res))
          (let ((y (car xs)))
            (if (equal? x y)
              (loop y (cdr xs) (cons y pack) res)
              (loop y (cdr xs) (list y) (cons pack res)))))))))

; 10
(define (encode xs)
  (map (lambda (x) (list (length x) (car x)))
       (pack xs)))

; 11
(define (encode-modified xs)
  (map (lambda (x) (let ((len (length x)) (x (car x)))
                     (if (eq? len 1) x (list len x))))
       (pack xs)))


; 12
(define (decode xs)
  (define (repeat n x res)
    (if (eq? n 0) res (repeat (- n 1) x (cons x res))))
  (let loop ((x null) (xs xs) (res '()))
    (if (empty? xs) (reverse res)
      (let ((x (car xs)))
        (loop x (cdr xs)
              (if (list? x)
                (repeat (car x) (cadr x) res)
                (cons x res)))))))

; 13 without simplification
(define (encode-direct-not-yet xs)
  (if (empty? xs) xs
    (let loop ((x (car xs)) (xs (cdr xs)) (c 1) (res '()))
      (if (empty? xs) (reverse (cons (list c x) res))
        (let ((y (car xs)))
          (if (equal? x y)
            (loop y (cdr xs) (+ c 1) res)
            (loop y (cdr xs) 1 (cons (list c x) res))))))))

; 13 simplified
(define (encode-direct xs)
  (define (simplify c x res)
    (cons (if (eq? c 1) x (list c x)) res))
  (if (empty? xs) xs
    (let loop ((x (car xs)) (xs (cdr xs)) (c 1) (res '()))
      (if (empty? xs) (reverse (simplify c x res))
        (let ((y (car xs)))
          (if (equal? x y)
            (loop y (cdr xs) (+ c 1) res)
            (loop y (cdr xs) 1 (simplify c x res))))))))

; 14
(define (dupli xs)
  (let loop ((xs xs) (res '()))
    (if (empty? xs) (reverse res)
      (let ((x (car xs)))
        (loop (cdr xs) (cons x (cons x res)))))))

; 15
(define (repli xs n)
  (define (repli x n res)
    (if (eq? n 0) res (repli x (- n 1) (cons x res))))
  (let loop ((xs xs) (res '()))
    (if (empty? xs) (reverse res)
      (let ((x (car xs)))
        (loop (cdr xs) (repli x n res))))))

; 16
(define (my-drop xs i)
  (let loop ((xs xs) (j i) (res '()))
    (cond ((empty? xs) (reverse res))
          ((eq? j 1) (loop (cdr xs) i res))
          (else (loop (cdr xs) (- j 1) (cons (car xs) res))))))

; 17
(define (my-split xs n)
  (let loop ((xs xs) (n n) (res '()))
    (cond ((empty? xs) (error "nope"))
          ((eq? n 1) (list (reverse (cons (car xs) res)) (cdr xs)))
          (else (loop (cdr xs) (- n 1) (cons (car xs) res))))))
