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
      (mathValue (searchState 'return state) state)
      (run (cdr parsetree) (stateGlobal (car parsetree) state)))))

;stateGlobal takes a statement and a state and returns the new state after
;evaluating the statement
(define stateGlobal
  (lambda (statement state)
    (cond
      ((eq? (car statement) 'while) (stateWhile (whileConditon statement) (whilebody statement) state))
      ((eq? (car statement) 'var) (stateDeclare (cdr statement) state))
      ((eq? (car statement) 'return) (stateReturn (cadr statement) state))
      (else (error "Incorrect syntax")))))

;stateDeclare takes a statement containing a variable and possibly an assignment
;and returns the new state with the variable declared
(define stateDeclare
  (lambda (statement state)
    (cond
      ((null? statement) state)
      ((null? (cdr statement)) (addToState (car statement) null (removeFromState (car statement) state)))
      (else (addToState (car statement) (mathValue (assignmentExp statement) state) (removeFromState (car statement) state))))))
;declare helpers
(define assignmentExp
  cadr)


;stateWhile takes a while loop condition, a loop body statement, and a stateEmpty
;and returns the
(define stateWhile
  (lambda (condition body state)
    (if (mathBoolean condition)
      (stateGlobal body state)
      state)))

;while helpers
(define whileConditon cadr)

(define whileBody caddr)

;mathBoolean evaluates a T/F condition, right left and eqSym should be set to null on call
(define mathBoolean
  (lambda (condition)
    (cond
      ((null? condition) #f)
      ((null? (cadddr condition) (boolEvaluateEquality condition)) ;if no 4th element, we should have some x == y to evaluate
      ;else, we have to mathValue the left and right sides, MUST eval left before right



;mathBoolean helpers



;stateReturn takes an expression and a state and adds the valuevalue of the expression
;to the state as the variable 'return
(define stateReturn
  (lambda (expression state)
    (addToState 'return (mathValue expression state) (removeFromState 'return state))))

;value takes an expression and returns it's mathematical value
(define mathValue
  (lambda (exp state)
    (cond
      ((null? exp) '())
      ((number? exp) exp)
      ((not (list? exp)) (searchState exp state))
      ((number? (operator exp)) exp)
      ((null? (cddr exp)) (- 0 (mathValue (operand1 exp) state)))
      ((eq? '+ (operator exp)) (+ (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '- (operator exp)) (- (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '* (operator exp)) (* (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '/ (operator exp)) (quotient (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '% (operator exp)) (remainder (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      (else (error "Unknown Operator")))))

;value helper
(define operator
  car)
(define operand1
  cadr)
(define operand2
  caddr)

;initial state, no variables declared or assigned
(define stateEmpty
  '(() ()))

;bindings to the names of variables in the state
(define nameBindings
  car)
;bindings to the values of variables in the state
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
      ((eq? var (caar state)) (caadr state))
      (else (searchState var (list (cdr (nameBindings state)) (cdr (valueBindings state))))))))

;removeFromState takes a var and removes it and returns the new state
;We should abstract some of the repetitive cars and cdrs
(define removeFromState
  (lambda (var state)
    (cond
      ((null? var) state) ;null var, return given state
      ((null? state) stateEmpty) ;null state, return stateEmpty
      ((or (null? (nameBindings state)) (null? (valueBindings state))) stateEmpty) ;null names or values
      ((eq? var (car (nameBindings state))) (cons (cdr (nameBindings state)) (cdr (valueBindings state)))) ;var match, return everything else
      (else (list (cons (car (nameBindings state)) (car (removeFromState var (list (cdr (nameBindings state)) (cdr (valueBindings state)))))) (cons (car (valueBindings state)) (cadr (removeFromState var (list (cdr (nameBindings state)) (cdr (valueBindings state)))))))))))
      ;(else (cons (cons (car (nameBindings state)) (car (valueBindings state))) (removeFromState var (cons (cdr (nameBindings state)) (cdr (valueBindings state))))))))) ;cons current to recurse into rest of list
