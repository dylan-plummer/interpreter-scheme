(require "simpleParser.scm")

;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename)
    (parser filename)))

(define evaulate
  (lambda (parsetree state)
    (if (null? parsetree)
      state
      (evaluate (cdr parsetree) (stateGlobal (car lis) state)))))

;stateGlobal takes a statement and a state and returns the new state after
;evaluating the statement
(define stateGlobal
  (lambda (statement state)
    (cond
      ((eq? (car statement) 'while) (stateWhile statement state))

;stateWhile takes a while loop condition, a loop body statement, and a stateEmpty
;and returns the
(define stateWhile
  (lambda (condition body state)


(define stateEmpty
  ('('() '())))

;addToState takes a variable and data and adds it to a state
(define addToState
  (lambda (var data state)
    (if (null? state)
      (cons (list var) (list data))
      (cons (cons var (car state)) (cons data (cadr state))))))

;searchVar takes a var and a state and returns associated data
(define searchVar
  (lambda (var state)
    (cond
      ((null? state) null)
      ((null? (cdr state)) null)
      ((eq? var (caar state)) (cadr state))
      (else (searchVar var (cdr state))))))

;removeVar takes a var and removes it and returns the new state
;We should abstract some of the repetitive cars and cdrs
(define removeVar
  (lambda (var state)
    (cond
      ((null? var) state ) ;null var, return given state
      ((null? state) '('() '()) ) ;null state, return stateEmpty
      ((eq? var (car (car state))) (cons (cadr (car state)) (cadr (cadr state)))) ;var match, return everything else
      (else (cons (cons (cdr (car state)) (cdr (cadr state))) (removeVar var (cons (cdr (car state)) (cdr (cadr state)))))))))