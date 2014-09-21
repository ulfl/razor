;; Copyright (c) 2014 Ulf Leopold
#lang racket

(require (planet "amb.ss" ("murphy" "amb.plt" 1 1)))
(require racket/generator)

;; Framework =============================================================
(define-syntax-rule (step description operation)
  (list (lambda () description) (lambda () operation)))

(define-syntax-rule (done) #f)

;; Model =================================================================
(define state (make-parameter (make-immutable-hash (list (cons 'account 0)))))
(define set (lambda (key val) (state (hash-set (state) key val))))
(define get (lambda (key) (hash-ref (state) key)))

(define deposit (lambda (orig amount) (set 'account (+ orig amount))))
(define withdraw (lambda (orig amount) (set 'account (- orig amount))))
(define balance (lambda () (get 'account)))

(define alice
  (list
   (step 'alice-get        (set 'alice-tmp (balance)))
   (step 'alice-deposit-10 (deposit (get 'alice-tmp) 10))
   (done)))

(define bob
  (list
   (step 'bob-get        (set 'bob-tmp (balance)))
   (step 'bob-withdraw-5 (withdraw (get 'bob-tmp) 5))
   (done)))

;; Execution =============================================================
(define scheduler
  (lambda (p1 p2 result)
    (parameterize ((state (state)))
      (cond
       ((and (not (car p1)) (not (car p2))) (list result (balance)))
       ((and (car p1) (car p2))
        (let* ((p1? (amb #t #f))
               (res (cond
                     (p1? (list (exec p1) (cdr p1) p2))
                     (else (list (exec p2) p1 (cdr p2))))))
          (match res
            ((list op p1_new p2_new)
             (scheduler p1_new p2_new (append result (list op)))))))
       ((and (car p1) (not (car p2)))
        (match (exec p1)
          (op (scheduler (cdr p1) p2 (append result (list op))))))
       ((and (car p2) (not (car p1)))
        (match (exec p2)
          (op (scheduler p1 (cdr p2) (append result (list op))))))))))

(define exec
  (lambda (p)
    (match (car p)
      ((list name op) (let ((n (name))) (op) n)))))

(define test
  (lambda ()
    (match (scheduler alice bob '())
      ((and (list ops balance) res)
       (amb-assert (not (eq? balance 5)))
       res))))

(define (run)
  (amb-collect (test)))
