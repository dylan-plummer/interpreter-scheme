(require "simpleParser.scm")

;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename)
    (run (parser filename) stateEmpty)))

(define run
  (lambda (parsetree state)
    (if (null? parsetree)
      (searchState 'return state)
      (run (cdr parsetree) (stateGlobal (car parsetree) state)))))

;stateGlobal takes a statement and a state and returns the new state after
;evaluating the statement
(define stateGlobal
  (lambda (statement state)
    (cond
      ((eq? (car statement) 'while) (stateWhile (whileConditon statement) (whilebody statement) state))
      ((eq? (car statement) 'return) (stateReturn (cadr statement) state))
      (else (error "Incorrect syntax")))))

;stateWhile takes a while loop condition, a loop body statement, and a stateEmpty
;and returns the
(define stateWhile
  (lambda (condition body state)
    (if (stateBoolean condition)
      (stateGlobal body state)
      state)))
;while helpers
(define whileConditon
  cadr)
(define whileBody
  caddr)

;stateReturn takes an expression and a state and adds the value of the expression
;to the state as the variable 'return
(define stateReturn
  (lambda (expression state)
    (addToState 'return (value expression) (removeFromState 'return state))))

;value takes an expression and returns it's value
(define value
  (lambda (exp)
    (cond
      ((null? exp) '())
      ((not (list? exp)) exp)
      ((number? (operator exp)) error)
      ((eq? '+ (operator exp)) (+ (value (operand1 exp)) (value (operand2 exp))))
      ((eq? '- (operator exp)) (- (value (operand1 exp)) (value (operand2 exp))))
      ((eq? '* (operator exp)) (* (value (operand1 exp)) (value (operand2 exp))))
      ((eq? '/ (operator exp)) (quotient (value (operand1 exp)) (value (operand2 exp))))
      ((eq? '% (operator exp)) (remainder (value (operand1 exp)) (value (operand2 exp))))
      (else (error "Unknown Operator")))))
;value helpers
(define operator
  car)
(define operand1
  cadr)
(define operand2
  caddr)

;initial state, no variables declared or assigned
(define stateEmpty
  '(() ()))

(define nameBindings
  car)
(define valueBindings
  cadr)

;addToState takes a variable and data and adds it to a state
(define addToState
  (lambda (var data state)
    (if (null? state)
      (list (list var) (list data))
      (list (cons var (nameBindings state)) (cons data (valueBindings state))))))

;searchState takes a var and a state and returns associated data
(define searchState
  (lambda (var state)
    (cond
      ((null? state) stateEmpty)
      ;((null? (cdr state)) null)
      ((eq? var (caar state)) (caadr state))
      (else (searchState var (list (cdr (name state)) (cdr (valueBindings state))))))))

;removeFromState takes a var and removes it and returns the new state
;We should abstract some of the repetitive cars and cdrs
(define removeFromState
  (lambda (var state)
    (cond
      ((null? var) state) ;null var, return given state
      ((null? state) stateEmpty) ;null state, return stateEmpty
      ((or (null? (nameBindings state)) (null? (valueBindings state))) stateEmpty)
      ((eq? var (car (car state))) (cons (cadr (car state)) (cadr (cadr state)))) ;var match, return everything else
      (else (cons (cons (cdr (car state)) (cdr (cadr state))) (removeFromState var (cons (cdr (car state)) (cdr (cadr state)))))))))
