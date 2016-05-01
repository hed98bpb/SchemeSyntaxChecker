;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-program
  (lambda (v)
    (cond
      [(null? v)
       #t]
      [(pair? v)
       (and (check-toplevel-form (car v))
            (check-program (cdr v)))]
      [else
       #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-toplevel-form
  (lambda (v)
    (cond
      [(is-definition? v)
       (check-definition v)]
      [else
       (check-expression v)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for definitions:
;;;;;;;;;;

;;; predicate:
(define is-definition?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'define))))

;;; 1st accessor:
(define define-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define define-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;
;;; the syntax checker proper for definitions:
;;;;;;;;;;

(define check-definition
  (lambda (v)
    (and (check-variable (define-1 v))
         (check-expression (define-2 v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for expressions:
;;;;;;;;;;
;;; predicate:


;;; Accessors ;;;
(define time-1
  (lambda (v)
    (list-ref v 1)))
(define if-1
  (lambda (v)
    (list-ref v 1)))
(define if-2
  (lambda (v)
    (list-ref v 2)))
(define if-3
(lambda (v)
    (list-ref v 3)))
(define cond-clause-1
  (lambda (v)
    (list-ref v 0)))
(define cond-clause-2
  (lambda (v)
    (list-ref v 1)))
(define cond-clause-3
  (lambda (v)
    (list-ref v 2)))
(define quasiquote-1
  (lambda (v)
    (list-ref v 1)))
(define begin-1
  (lambda (v)
    (list-ref v 1)))
(define quote-1
  (lambda (v)
    (list-ref v 1)))
;;; Predicates ;;;
(define is-application?
(lambda (v)
  (and (list-strictly-longer-than? v 0)
       (not (member (list-ref v 0) (list 'define 'time 'if 'cond 'else 'case 'and 'or 'let 'let* 'letrec 'begin 'quote 'quasiquote 'unquote 'unquote-splicing 'lambda 'trace-lambda))))))
(define is-time?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'time))))
(define is-if?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (equal? (car v) 'if))))
(define is-cond?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'cond))))
(define is-case?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'case))))
(define is-and?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'and))))
(define is-or?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'or))))
(define is-let?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'let))))
(define is-letstar?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'let*))))
(define is-letrec?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'letrec))))
(define is-begin?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'begin))))
(define is-quote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'quote))))		 
(define is-quasiquote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'quasiquote))))
(define is-lambda?
  (lambda (v)
         (or (and (proper-list-of-given-length? v 3)
                   (equal? (car v) 'lambda))
             (and (proper-list-of-given-length? v 4)
                  (equal? (car v) 'trace-lambda)))))		

;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;
(define check-expression
  (lambda (v)
    (cond
      [(number? v)
       #t]
      [(boolean? v)
       #t]
      [(char? v)
       #t]
      [(string? v)
       #t]
      [(symbol? v)
       (check-variable v)]
      [(is-time? v)
       (check-time-expression v)]
      [(is-if? v)
       (check-if-expression v)]
      [(is-cond? v)
       (check-cond-expression v)]
      [(is-case? v)
       (check-case-expression (cdr v))]
      [(is-and? v)
       (check-and-expression (cdr v))]
      [(is-or? v)
       (check-or-expression (cdr v))]
      [(is-let? v)
       (check-let-expression (cdr v))]
      [(is-letstar? v)
       (check-letstar-expression (cdr v))]
      [(is-letrec? v)
       (check-letrec-expression (cdr v))]
      [(is-begin? v)
       (check-begin-expression v)]
      [(is-quote? v)
       (check-quote-expression  v)]
      [(is-quasiquote? v)
       (check-quasiquote-expression (cdr v))]
      [(is-lambda? v)
       (check-lambda-abstraction v)]
      [(is-application? v)
       (check-application v)]
      [else
       #f])))

(define check-application
  (lambda (v)
        (and (pair? v)
             (check-expression (car v))
             (check-expression* (cdr v)))))

(define check-expression*
  (lambda (v)
    (or (null? v)
        (and (pair? v)
             (check-expression (car v))
             (check-expression* (cdr v))))))
 
(define check-expression+
  (lambda (v)
    (and (pair? v)
         (check-expression (car v))
         (check-expression* (cdr v)))))

(define check-variable
  (lambda (v)
    (and (symbol? v)
         (not (member v (list 'define 'time 'if 'cond 'else 'case 'and 'or 'let 'let* 'letrec 'begin 'quote 'quasiquote 'unquote 'unquote-splicing 'lambda 'trace-lambda))))))

(define check-variable*
  (lambda (v)
    (or (null? v)
        (check-variable v)
        (and (pair? v)
             (check-variable (car v))
             (check-variable* (cdr v))))))

(define check-variable+
  (lambda (v)
        (and (pair? v)
             (check-variable (car v))
             (check-variable* (cdr v)))))

(define check-time-expression
  (lambda (v)
    (check-expression (time-1 v))))

(define check-if-expression
  (lambda (v)
    (and (check-expression (if-1 v))
         (check-expression (if-2 v))
         (check-expression (if-3 v)))))

(define check-cond-expression
  (lambda (v)
      (letrec ([visit
                 (lambda (x)
                   (cond [(and (pair? x)
                            (pair? (car x))
                            (null? (cdr x))
                            (equal? (caar x) 'else)
                            (list? (cdar x))
                            (= 1 (length (cdar x)))
                            (check-expression (cadar x)))
                          #t]
                         [(and (pair? x)
                               (check-cond-clause (car x))
                               (visit (cdr x)))
                          #t]
                         [else
                          #f]))])
         (visit (cdr v)))))

(define check-cond-clause
  (lambda (e)
    (cond [(and (list? e)
                (<= (length e) 2)
                (check-expression+ e))
           #t]
          [(and (pair? e)
             (check-expression (car e))
             (pair? (cdr e))
             (equal? (cadr e) '=>)
             (pair? (cddr e))
             (check-expression (caddr e))
             (null? (cdddr e)))
           #t]
          [else
           #f])))

(define check-case-expression
  (lambda (v)
    (cond
      [(and (pair? v)
            (check-expression (car v))
            (letrec ([visit
              (lambda (x)
                (and (pair? x)
                     (or (and (null? (cdr x))
                              (proper-list-of-given-length? (car x) 2)
                              (equal? (caar x) 'else)
                              (list? (cdar x))
                              (= 1 (length (cdar x)))
                              (check-expression (cadar x)))
                         (and (proper-list-of-given-length? (car x) 2)
                              (check-quotation* (caar x))
                              (check-expression+ (cdar x))
                              (visit (cdr x))))))])
              (visit (cdr v))))
       #t]
      [else
       #f])))

(define check-and-expression
  (lambda (v)
    (check-expression* v)))
(define check-or-expression
  (lambda (v)
    (check-expression* v)))

(define check-let-expression
  (lambda (v)
    (cond
      [(and (pair? v)
            (letrec ([visit
                      (lambda (x y)
                        (or (null? x)
                            (and (pair? x)
                                 (pair? (car x))
                                 (check-variable (caar x))
                                 (pair? (cdar x))
                                 (null? (cddar x))
                                 (check-expression (cadar x))
                                 (not (member (caar x) y))
                                 (visit (cdr x) (cons (caar x) y)))))])
              (visit (car v) (list)))
            (check-expression+ (cdr v)))
       #t]
      [else
       #f])))

(define check-letstar-expression
  (lambda (v)
    (cond
      [(and (pair? v)
            (letrec ([visit
                      (lambda (x)
                        (or (null? x)
                            (and (pair? x)
                                 (pair? (car x))
                                 (check-variable (caar x))
                                 (pair? (cdar x))
                                 (null? (cddar x))
                                 (check-expression (cadar x))
                                 (visit (cdr x)))))])
              (visit (car v)))
            (check-expression+ (cdr v)))
       #t]
      [else
       #f])))

(define check-letrec-expression
  (lambda (v)
    (cond
      [(and (pair? v)
            (letrec ([visit
                      (lambda (x y)
                        (or (null? x)
                            (and (pair? x)
                                 (pair? (car x))
                                 (not (null? (caar x)))
                                 (check-variable (caar x))
                                 (pair? (cdar x))
                                 (null? (cddar x))
                                 (not (member (caar x) y))
                                 (check-lambda-abstraction (cadar x))
                                 (visit (cdr x) (cons (caar x) y)))))])
              (visit (car v) (list)))
            (check-expression+ (cdr v)))
       #t]
      [else
       #f])))

(define check-begin-expression
  (lambda (v)
    (cond
      [(check-expression+ (cdr v))
       #t]
      [else
       #f])))

(define check-quote-expression
  (lambda (v)
    (check-quotation (quote-1 v))))
	
(define check-quasiquote-expression
  (lambda (v)
    (letrec ([visit (lambda (v number-of-nestings)
                      (errorf 'check-quasiquote-expression "not implemented yet: ~s" v))])
      (visit (quasiquote-1 v) 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-quotation
  (lambda (v)
    (cond
      [(number? v)
       #t]
      [(boolean? v)
       #t]
      [(char? v)
       #t]
      [(string? v)
       #t]
      [(symbol? v)
       #t]
      [(null? v)
       #t]
      [(and (pair? v)    
            (check-quotation (car v))
            (check-quotation (cdr v)))]
      [else
      #f])))

(define check-quotation*
  (lambda (v)
    (or (null? v)
        (and (pair? v)
             (check-quotation (car v))
             (check-quotation (cdr v))))))

(define check-quasiquote-expression
  (lambda (v)
    (check-quasiquote v 0)))

(define check-quasiquote
  (lambda (v i)
    (cond
      [(and (= i 0)
            (pair? v)
            (or (equal? (car v) 'unquote)
                (equal? (car v) 'unquote-splicing))
            (check-expression (cdr v)))
       #t]
      [(or (integer? v)
           (boolean? v)
           (char? v)
           (string? v)
           (symbol? v)
           (null? v)
           (and (pair? v)
                (not (or (equal? (car v) 'unquote)
                         (equal? (car v) 'unquote-splicing)))
                (check-quasiquote (car v) i)
                (check-quasiquote (cdr v) i))
           (and (pair? v)
                (equal? (car v) 'quasiquote)
                (check-quasiquote (cdr v) (+ i 1))))
       #t]
      [(and (> i 0)
            (and (pair? v)
                 (or (equal? (car v) 'unquote)
                     (equal? (car v) 'unquote-splicing))
                 (check-quasiquote (cdr v) (- i 1))))
       #t]
      [else
       #f])))

(define check-variable*-distinct
  (lambda (v l)
    (or (null? v)
        (and (not (member v l))
             (check-variable v))
        (and (pair? v)
             (not (member (car v) l))
             (check-variable (car v))
             (check-variable*-distinct (cdr v) (cons (car v) l))))))

(define check-variable+-distinct
  (lambda (v l)
        (and (pair? v)
             (check-variable (car v))
             (not (member (car v) l))
             (check-variable*-distinct (cdr v) (cons (car v) l)))))

(define check-lambda-abstraction
  (lambda (v)
    (or (and (pair? v)
             (equal? (car v) 'lambda)
             (pair? (cdr v))
             (check-lambda-formals (cadr v))
             (check-expression+ (cddr v)))
        (and (pair? v)
             (proper-list-of-given-length? v 4)
             (equal? (car v) 'trace-lambda)
             (check-quotation (cadr v))
             (check-lambda-formals (caddr v))
             (check-expression+ (cdddr v))))))

(define check-lambda-formals
  (lambda (v)
    (or (check-variable v)
        (check-variable*-distinct v '())
        (and (pair? v)
             (check-variable+-distinct (car v) (cdr v))
             (check-variable (cdr v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
;;; auxiliaries:
;;;;;;;;;;
(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v)
                                      (- i 1)))))])
      (if (>= n 0)
          (visit v n)
          (errorf 'list-strictly-longer-than? "negative length: ~s" n)))))

;;; reads an entire file as a list of Scheme data
;;; use: (read-file "filename.scm")
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))
;;; interface:
(define check-file
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (errorf 'check-file "not a string: ~s" filename))))

;;; proper-list-length
(define proper-list-length
  (lambda (xs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          0
                          (+ (visit (cdr ws)) 1)))])
      (visit xs))))
(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

(define trace-all
   (lambda ()
       (trace check-cond-expression
              check-expression
              check-time-expression
              check-if-expression
              check-cond-clause
              check-and-expression
              check-begin-expression
              check-lambda-abstraction
              check-let-expression
              check-case-expression
              check-letstar-expression
              check-letrec-expression
              check-quasiquote
              check-lambda-formals
              check-variable+
              check-variable*
              check-variable 
              check-file
              check-expression*
              check-expression+
              check-application
              if-1
              quote-1
              define-1
              )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define well-formed-expressions
  '(

    ;;; numbers
    0
    2
    -3
    2/3
    -2/3
    2.3
    -4.5

    ;;; characters
    #\a
    #\b
    #\space

    ;;; strings
    ""
    "hello world"

    ;;; symbols
    x
    y
    123f

    ;;; time
    (time 1)
    (time #t)
    (time (if 1 2 3))
    
    ;;; if
    (if 1 2 3)
    (if (if 1 2 3) 4 5)
    (if (if 1 2 3)
        (if 4 5 6)
        (if 7 8 9))

    ;;; cond
    (cond [else #f])
    (cond [#t] [(if 1 2 3) "hej"] [3] [else 5])
    (cond [(if 1 2 3) "hej"] [else 5])
    (cond [(cond [else 3]) => #t] [else #f])

    ;;; case
    (case 1 [else #f])
    (case 1 [(1 2 3 4 5) "hej"] [else #f])
    (case "hej" [(#f "hm") (if 1 2 3)] [else #f])
    (case #t [((() . 5)) #t] [else 5])
    (case (case #t [(5 4 3) #t] [else "hej"]) [else #t])

    ;;; and
    (and )
    (and (and 1 2) 3 4)
    (and (and 1 2 3)
         (and 4 5 6)
         (and 7 8 9))
    
    ;;; or
    (or )
    (or (or 1 2) 3 4)
    (or (or 1 2 3)
         (or 4 5 6)
         (or 7 8 9))

    ;;; let
    (let ([x #t]) "hej")
    (let () 5)
    (let ([x #f] [y #f] [z "hmm"]) #\a)
    (let ([x (let ([y #f] [z 3]) "hej")] [d 5]) 2)


    ;;; let*
    (let* ([x #t]) 4)
    (let* () "hej")
    (let* ([x #t] [x 4] [y 5]) "hmm")
    (let* ([x (let* ([y #f] [z 3]) "hej")] [d 5]) 2)

    ;;; letrec
    (letrec ([x (lambda y y)]) #t)
    (letrec ([x (lambda (x y z) "hej")] [y (lambda x 5)]) "hej")
    (letrec ([x (trace-lambda hej x #t)]) "hmm")
    (letrec ([x (trace-lambda hm (x y z . t) #t)]
             [y (lambda (x y z . t) #t)]
             [z (trace-lambda var (x y z) "hej")])
      (if 3 4 5))

    ;;; begin
    (begin 1)
    (begin 1 2)
    (begin 1 2 3)
    (begin
       (begin 1)
       (begin 1 2)
       (begin 1 2 3))

    ;;; quote
    (quote 4)
    (quote #t)
    (quote #\a)
    (quote "hej")
    (quote (5 . ()))
    (quote x)
    (quote ())
    (quote (5 . #t))
    (quote (5 . (4 . (3 . 2))))
    (quote (5 4 3 2))


    ;;; quasiquote
    (quasiquote 5)
    (quasiquote #t)
    (quasiquote #\a)
    (quasiquote "hej")
    (quasiquote x)
    (quasiquote ())
    (quasiquote (unquote x))
    (quasiquote (unquote-splicing x))
    (quasiquote (5 . 6))
    (quasiquote (quasiquote 5))
    (quasiquote (unquote #t))
    (quasiquote (unquote-splicing "hej"))
    (quasiquote (5 . ()))
    (quasiquote (unquote (quasiquote (()))))
    (quasiquote (quasiquote (quasiquote (unquote (unquote (5))))))
    (quasiquote (5 4 3 2))

    ;;; lambda-abstraction
    (lambda x 5)
    (lambda (x y z) "hej")
    (lambda (x y z . t) #t)
    (trace-lambda hej x #t)
    (trace-lambda var (x y z) "hej")
    (trace-lambda hm (x y z . t) #t)


    ;;; etc.        

    ))

(define test-well-formed-expressions
  (lambda ()
    (andmap (trace-lambda well-formed (e)
              (check-expression e))
            well-formed-expressions)))

(define test-well-formed-expressions-silently
  (lambda ()
    (andmap (lambda (e)
              (check-expression e))
            well-formed-expressions)))



(define ill-formed-expressions
  '(

   ; time
    (time 1 2)
    (time)
    (time 1 . 2)
    (time 1 2 . 3)    
    
   ; if
    (if)
    (if 1)
    (if 1 2)
    (if 1 2 3 4)
    (if . 1)
    (if 1 . 2)
    (if 1 2 . 3)
    (if 1 2 3 . 4)

   ; cond
    (cond [#t])
    (cond [1 2 3])
    (cond [1 2 3] [else #t])
    (cond [else ])
    (cond [else ()])
    (cond [3 => 4 5] [else #t])
    (cond [else 4 5])

   ; case
    (case [else 4])
    (case 4 [else ])
    (case () [2 #t] [else "hej"])
    (case 2 [4 (3)] [else #f])
    (case 3 4 [() 3] [else 2])
    (case 2 [() 3 2] [else #t])
    (case 2 [() 3] [else #t #f])

   ; and
    (and . 1)
    (and 1 . 2)
    (and 1 2 . 3)

   ; or
    (or . 1)
    (or 1 . 2)
    (or 1 2 . 3)

   ; let
    (let () 3 4)
    (let ([x ]) #t)
    (let ([#t]) 3)
    (let ([#t x]) 3)
    (let ([define 5]) #t)
    (let ([x 5] 5) #t)
    (let ([x 5] [y 7] [z 6] 4) #t)
    (let ([x 5] [y 7] [x 6]) #t)

   ; letstar
    (let* () 3 4)
    (let* ([x ]) #t)
    (let* ([#t]) 3)
    (let* ([#t x]) 3)
    (let* ([define 5]) #t)
    (let* ([x 5] 5) #t)
    (let* ([x 5] [y 7] [x 6] 4) #t)

   ; letrec
    (letrec ())
    (letrec () . 5)
    (letrec ([x (lambda x 5)] 3) #t)
    (letrec ([x] (lambda x 5)) #t)
    (letrec ([(lambda x 5)] 2) #t)
    (letrec ([x]) 5)
    (letrec ([(lambda x 5)]) 5)
    (letrec ([x] [(lambda x 5)]) 5)
    (letrec ([x (lambda x 5)] [y (lambda x 5)] [x (lambda x 5)]) #\a)
   ; begin
    (begin)
    (begin . 3)
    (begin 1 2 . 3)

   ; quote
    (quote "hej" 5)
    (quote 4 5)
    (quote 3 4 5)
    (quote 4 . 5)
    (quote 3 4 . 5)
    
   ; quasiquote
    (quasiquote "hej" 5)
    (quasiquote 4 5)
    (quasiquote 3 4 5)
    (quasiquote 4 . 5)
    (quasiquote 3 4 . 5)

   ; lambda-abstraction
    (lambda a)
    (lambda #t)
    (lambda 3 #t)
    (lambda (x 5 y) #f)
    (lambda (x y z x) 5)
    (trace-lambda a)
    (trace-lambda 3)
    (trace-lambda 3 #t)
    (trace-lambda hej 3 #t)
    
    
    ))

(define test-ill-formed-expressions
  (lambda ()
    (andmap (trace-lambda ill-formed (e)
              (not (check-expression e)))
            ill-formed-expressions)))

(define test-ill-formed-expressions-silently
  (lambda ()
    (andmap (lambda (e)
              (not (check-expression e)))
            ill-formed-expressions)))

(define test
  (lambda ()
    (and (test-well-formed-expressions)
         (test-ill-formed-expressions))))

(define silent-test
  (lambda ()
    (and (test-well-formed-expressions-silently)
         (test-ill-formed-expressions-silently))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing samples ;;;
(define andmap
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          #t
                          (and (p (car ws))
                               (visit (cdr ws)))))])
      (visit vs))))

(define test-well-formed-expressions
  (lambda ()
    (andmap (trace-lambda well-formed (e)
              (check-toplevel-form e))
            well-formed-expressions)))

(define test-well-formed-expressions-silently
  (lambda ()
    (andmap (lambda (e)
              (check-toplevel-form e))
            well-formed-expressions)))

(define test-ill-formed-expressions
  (lambda ()
    (andmap (trace-lambda ill-formed (e)
              (not (check-toplevel-form e)))
            ill-formed-expressions)))

(define test-ill-formed-expressions-silently
  (lambda ()
    (andmap (lambda (e)
              (not (check-toplevel-form e)))
            ill-formed-expressions)))

(define test-well-formed-definitions
  (lambda ()
    (andmap (trace-lambda well-formed (e)
              (check-toplevel-form e))
            well-formed-definitions)))

(define test-well-formed-definitions-silently
  (lambda ()
    (andmap (lambda (e)
              (check-toplevel-form e))
            well-formed-definitions)))

(define test-ill-formed-definitions
  (lambda ()
    (andmap (trace-lambda ill-formed (e)
              (not (check-toplevel-form e)))
            ill-formed-definitions)))

(define test-ill-formed-definitions-silently
  (lambda ()
    (andmap (lambda (e)
              (not (check-toplevel-form e)))
            ill-formed-definitions)))

(define test
  (lambda ()
      (and (test-well-formed-expressions)
           (test-ill-formed-expressions)
           (test-well-formed-definitions)
           (test-ill-formed-definitions))))

(define silent-test
  (lambda ()
      (and (test-well-formed-expressions-silently)
           (test-ill-formed-expressions-silently)
           (test-well-formed-definitions-silently)
           (test-ill-formed-definitions-silently))))

;;;;;
(define well-formed-definitions
  '(
    (define asd 123)
    (define qqq asd)
    (define test-expressions
      (lambda ()
        (letrec ([visit (lambda (ws)
                          (if (null? ws)
                              #t
                              (and (if (check-expression (car ws))
                                       #t
                                       (begin (printf "!!! fa ~n" (car ws)) #f))
                                   (visit (cdr ws)))))])
          (visit well-formed-expressions))))
    (define a23 (and))

    )
)



(define well-formed-expressions
  '(

    ;;; numbers
    0
    2
    -3
    2/3
    -2/3
    2.3
    -4.5

    ;;; characters
    #\a
    #\b
    #\space

    ;;; strings
    ""
    "hello world"

    ;;; symbols
    x
    y
    123f

    ;;; time
    (time 1)
    (time #t)
    (time (if 1 2 3))
    
    ;;; if
    (if 1 2 3)
    (if (if 1 2 3) 4 5)
    (if (if 1 2 3)
        (if 4 5 6)
        (if 7 8 9))

    ;;; cond
    (cond [else #f])
    (cond [#t] [(if 1 2 3) "hej"] [3] [else 5])
    (cond [(if 1 2 3) "hej"] [else 5])
    (cond [(cond [else 3]) => #t] [else #f])
    (cond [2 2] [else #t])

    ;;; case
    (case 1 [else #f])
    (case 1 [(1 2 3 4 5) "hej"] [else #f])
    (case "hej" [(#f "hm") (if 1 2 3)] [else #f])
    (case #t [((() . 5)) #t] [else 5])
    (case (case #t [(5 4 3) #t] [else "hej"]) [else #t])
    (case e2 [(() () () ()) e1] [else e2])
    (case e1 [((time . if)) (e 2)] [("hej" 1 #t #\space () asd case) (time 1)] [else #f])

    ;;; and
    (and )
    (and (and 1 2) 3 4)
    (and (and 1 2 3)
         (and 4 5 6)
         (and 7 8 9))
    
    ;;; or
    (or )
    (or (or 1 2) 3 4)
    (or (or 1 2 3)
        (or 4 5 6)
        (or 7 8 9))

    ;;; let
    (let ([x #t]) "hej")
    (let () 5)
    (let ([x #f] [y #f] [z "hmm"]) #\a)
    (let ([x (let ([y #f] [z 3]) "hej")] [d 5]) 2)
    (let ([x (let ([y #f]) y)] [y 2]) 2)
    (let ([x 2] [y (let ([x 1] [y 2]) a)]) b)
    (let ([e e] [d e] [f d]) (d e f))


    ;;; let*
    (let* ([x #t]) 4)
    (let* () "hej")
    (let* ([x #t] [x 4] [y 5]) "hmm")
    (let* ([x (let* ([y #f] [z 3]) "hej")] [d 5]) 2)
    (let* ([x x] [x x]) (time 2))

    ;;; letrec
    (letrec ([x (lambda z y)]) #t)
    (letrec ([x (lambda (x y z) "hej")] [y (lambda x 5)]) "hej")
    (letrec ([x (trace-lambda hej x #t)]) "hmm")
    (letrec ([x (trace-lambda hm (x y z . t) #t)]
             [y (lambda (x y z . t) #t)]
             [z (trace-lambda var (x y z) "hej")])
      (if 3 4 5))
    (letrec ([e (lambda e e)] [f (lambda e (e f))]) e)

    ;;; begin
    (begin 1)
    (begin 1 2)
    (begin 1 2 3)
    (begin
      (begin 1)
      (begin 1 2)
      (begin 1 2 3))
    (begin ((3)) e)
    (begin ((2) (4)) e)

    ;;; quote
    (quote 1)
    (quote #f)
    (quote #\c)
    (quote "hej")
    (quote asd)
    (quote ())
    (quote (1 . 2))
    (quote (quote (quote 1)))
    (quote (a b "f" #t 2 3 1 (lambda x . y)))

    ;;; quasiquote
    `1
    `#f
    `#\d
    `"med"
    `dig
    `define
    `()
    `(ab ,v)
    `(#f 1 ,4 2 . 3)
    ``````````,,,,,a
    ``(a ,b c ,(d e ,#f ,(lambda (c) (+ c 2))) 3 `(x ,y ,,z v))
    `(a ,1)
    `(1 `(2 (unquote-splicing 3)) ,4 ,(5 `6 '3))
    `(a ,('c ``(1 ,time ,define ,(,1 time define)) 2) x)

    ;;; lambda-abstraction
    (lambda x 5)
    (lambda (x y z) "hej")
    (lambda (x y z . t) #t)
    (trace-lambda hej x #t)
    (trace-lambda var (x y z) "hej")
    (trace-lambda hm (x y z . t) #t)
    (trace-lambda time (x) "hej")
    (trace-lambda 1 (x) 2) ; Change in the BNF, used to be disallowed: Only symbols, not entire quotation!
    (trace-lambda (1 2 3) (x) 2)
    (trace-lambda "hej" v v)
    (trace-lambda #t v v)
    (trace-lambda #\space v v)
    

    ;;; lambda-formals
    ;;  x
    ;;  (x y z)
    ;;  (x y z . t)

    ;;; etc.        

    ))

(define ill-formed-definitions
  '(
    (define a)
    (define)
    (define a b c)
    (define 1 2)
    (define (if 1 2 3) 4)
    (define #t #f)
    (define "s" 1)
    (define #\c 2)
    (define define 1)
    (define time 2)
    (define if 1 2 3)
    (define if 3)
    (define cond 4)
    (define else 5)
    (define case 6)
    (define and 7)
    (define or 8)
    (define let 9)
    (define let* 10)
    (define () 1)
    (define letrec 2)
    (define begin 4)
    (define quote 4)
    (define quasiquote 0)
    (define unquote 9)
    (define unquote-splicing 0)
    (define lambda #t)
    (define trace-lambda #f)
    (define x and)
    (define y or)
    (define z quasiquote)
    (define a (lambda (v) (+ v lambda)))
    (define a (cond [#f 1] [a b]))

    ))

(define ill-formed-expressions
  '(
    ()
    ;;; time
    (time)
    (time 1 2)
    (time . 2)
    (time 1 . 2)
    (time time)
    (time (time))

    ;;; if
    (if)
    (if 1)
    (if 1 2)
    (if 1 2 3 4)
    (if . 1)
    (if 1 . 2)
    (if 1 2 . 3)
    (if 1 2 3 . 4)
    
    ;;; cond
    (cond [else])
    (cond [e1 e2 e3] [else e4])
    (cond [else e1] [else e2])
    (cond [else 1] [#t "abd"])
    (cond [] [else #t])
    (cond [if 1 2 3] [time 2] [else 2])
    (cond [(if 1 2)] [else 4])
    (cond [time] [else "yeah!"])
    (cond [#t #f] . [else #t])
    (cond [1 2] . 3)
    (cond [1 . 3] [else 2])
    (cond [2 3] [else . #t])

    ;;; case
    (case)
    (case e1)
    (case [else e1])
    (case e2 [else e3 e4])
    (case s [1 2] [else #f])
    (case 1 [] [else 1])
    (case 2 [(1) 2] [else])
    (case 1 [(1 2 3) (time e)] [else e1 e2])
    (case (if 2) [else 1])
    (case . 2)
    (case 1 . 2)
    (case 4 [() #f] [(asd) (and)] . #t)
    (case 1 [() . 3] [else 2])
    (case #t [("hej") . 2] [else #f])
    (case 3 [else . 4])

    ;;; and
    (and and)
    (and (define x 1))
    (and 1 else #t)
    (and 1 2 3 4 (if 1 2))
    (and quasiquote)
    (and unquote-splicing)
    (and time 1)
    (and 1 2 . #t)

    ;;; or
    (or or)
    (or (define x 2))
    (or if 1 2 3)
    (or time 2)
    (or 2 3 (and and))
    (or #t #t (quote))
    (or 1 2 #t . #f)
    (or . #t)

    ;;; let
	(let)
    (let e1)
    (let ([x 1] [y 2]))
    (let () let)
    (let ([x 1] [y 2] [z 3] [y 4]) e1)
    (let ([x 1]) (define y 2))
    (let ([if 3]) e2)
    (let ([x (define y 2)]) e2)
    (let ([c (if 1 2)]) e3)
    (let ([x 2]) (time 1 2))
    (let ([x 1] . [y 2]) 3)
    (let 3 . 4)
    (let ([x 2] [y 3]) . e)
    (let ([x . 2]) e)
    (let ([x 3 3]) e)
    (let ([1 e]) d)
    (let (a 3) #t)
    
    ;;; let*
    (let*)
    (let* e1)
    (let* () ())
    (let* () e1 e2)
    (let* ([x 1]) (define y 2))
    (let* ([if 3]) e2)
    (let* ([x (define y 2)]) e2)
    (let* ([c (if 1 2)]) e3)
    (let* ([x 2]) (time 1 2))
    (let* ([x 1] . [y 2]) 3)
    (let* ([x 1]) . #f)
    (let* ([x . 2]) #t)
    (let* ([3 2]) 2)
    (let* (v e) r)
    (let* (a . 3) 2)
    (let* ([a 4] . 2) r)

    ;;; letrec
    (letrec)
    (letrec e1)
    (letrec () ())
    (letrec ())
    (letrec ([x 2]) e2)
    (letrec ([if (lambda (v))]) e3)
    (letrec ([x (lambda (v) (define y 2))]) e3)
    (letrec ([x (lambda v a)] [y (lambda q e)] [x (lambda z 3)]) e1)
    (letrec ([x (lambda v a) (lambda u b)]) e1)
    (letrec ([x . 3]) e2)
    (letrec ([x 3]) . e2)
    (letrec ([x . (lambda v v)]) e)
    (letrec (x (lambda v v)) e)

    ;;; begin
    (begin)
    (begin . 3)
    (begin 1 2 . 3)
    (begin (define x 2))
    (begin () 2)
    (begin 1 2 3 else)
    (begin (time 2) 3 (if 1 2))


    (begin (1 . 3) e)

    ;;; quotation
    (quote) ; Can't seem to find more!

    ;;; quasiquote
    (quasiquote)
    `,,a
    `(a b ,(define x 2))
    ``````,,,(a b `(c ,,,,,,,d))
    `(a b ,(`(,if ,1))) 
    `(unquote-splicing (,a))
    `(unquote-splicing (time))
    `(a `b `,,(define c 1) a)
    `(1 `2 ,(`a ,b))
    `(a 2 (,time))
    `(1 ,(2 . 3))
    `(a b '(x ,,,y z) c)
    
    ;;; lambda
    (lambda)
    (lambda v)
    (lambda v ())
    (lambda if 2)
    (lambda (time 2) e1)
    (lambda v time)
    (lambda 1 2 3)
    (lambda 3 (time))
    (lambda (a b c . if) e1)
    (lambda . 2)
    (lambda (c . time) e)
    (lambda c . d)
    (lambda (v) 1 . 3)

    ;;; trace-lambda
    (trace-lambda)
    (trace-lambda 1)
    (trace-lambda v e1)
;    (trace-lambda (a b c) a v) ; Change in the BNF
;    (trace-lambda 1 a v) ; Change in the BNF
    (trace-lambda v a time)
    (trace-lambda tl else #t)
;    (trace-lambda #t a 3) ; Change in the BNF
    (trace-lambda e 2 3 4)
    (trace-lambda e 2 . 3)
    (trace-lambda e (a b . 3) 3)
    (trace-lambda e (c d time e) #t)
    (trace-lambda f (v) (begin))
    (trace-lambda . d)
    (trace-lambda v . 3)
    (trace-lambda eq (v) . 3)

    

    ;;; application
    (1 2 . 3)
    (#t 2 4 (define x 2) 3)
    (2 3 'a (time))
    (1 2 . (define x 2))

    ))

  

