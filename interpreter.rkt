(require "simpleParser.scm")

;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename)
    (run (parser filename) stateEmpty)))

(define run
  (lambda (parsetree state)
    ;(display state) (newline)
    (if (null? parsetree)
      state
      (run (cdr parsetree) (stateGlobal (car parsetree) state)))))

;stateGlobal takes a statement and a state and returns the new state after
;evaluating the statement
(define stateGlobal
  (lambda (statement state)
    (display statement) (newline)
    (cond
      ((eq? (car statement) 'while) (stateWhile (whileConditon statement) (whilebody statement) state))
      ((eq? (car statement) 'var) (stateDeclare (cdr statement) state))
      ((eq? (car statement) 'return) (stateReturn (cadr statement) state))
      ((eq? (car statement) '=) (stateAssign (cdr statement)  state))
      (else (error "Incorrect syntax")))))

;stateDeclare takes a statement containing a variable and possibly an assignment
;and returns the new state with the variable declared
(define stateDeclare
  (lambda (statement state)
    (cond
      ((null? statement) state)
      ((null? (cdr statement)) (addToState (variable statement) null (removeFromState (variable statement) state)))
      (else (addToState (variable statement) (mathValue (assignmentExp statement) state) (removeFromState (variable statement) state))))))

;stateAssign takes a statement containing a variable and an expression and returns
;the new state with the variable assigned the value of the expression
(define stateAssign
  (lambda (statement state)
    ;(display statement) (newline)
    (cond
      ((null? statement) state)
      (else (addToState (variable statement) (mathValue (assignmentExp statement) state) (removeFromState (variable statement) state))))))
;assignment helpers
(define assignmentExp
  cadr)
(define variable
  car)

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

;stateReturn takes an expression and a state and adds the value of the expression
;to the state as the variable 'return
(define stateReturn
  (lambda (expression state)
    (mathValue expression state)))

;value takes an expression and returns it's mathematical or boolean value
(define mathValue
  (lambda (exp state)
    (cond
      ;null/error checks
      ((null? exp) exp)
      ((number? exp) exp) ;no futher recursion needed, return number value
      ((not (list? exp)) (searchState exp state)) ;not  number, yet not a list...must be a variable!
      ((number? (operator exp)) (error "Invalid expression")) ;the expression has no operator :(
      ;&&/||/! evaluation, needs to be in format (operator bool bool) else bad logic
      ((eq? '&& (operator exp)) (and (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '|| (operator exp)) (or (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '!  (operator exp)) (and (not (mathValue (operand1 exp) state)) (not (mathValue (operand2 exp) state)))) ;Here I'm implementing ! as (and (not cond1) (not cond2))
      ;boolean evaluation
      ((eq? '== (operator exp)) (= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '!= (operator exp)) (not (= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state))))
      ((eq? '<= (operator exp)) (>= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '>= (operator exp)) (>= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '<  (operator exp))  (< (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '>  (operator exp))  (> (mathValue (operand exp) state) (mathValue (operand2 exp) state)))
      ;math evaluation
      ((null? (cddr exp)) (- 0 (mathValue (operand1 exp) state))) ;unary negation
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
(define boolValue
  (lambda (bool)
    (if bool
        'true
        'false)))

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
    ;(display state) (newline)
    (cond
      ((null? var) state) ;null var, return given state
      ((null? state) state) ;null state, return stateEmpty
      ((or (null? (nameBindings state)) (null? (valueBindings state))) state) ;null names or values
      ((eq? var (car (nameBindings state))) (list (cdr (nameBindings state)) (cdr (valueBindings state)))) ;var match, return everything else
      (else (list (cons (car (nameBindings state)) (car (removeFromState var (list (cdr (nameBindings state)) (cdr (valueBindings state)))))) (cons (car (valueBindings state)) (cadr (removeFromState var (list (cdr (nameBindings state)) (cdr (valueBindings state)))))))))))
      ;(else (cons (cons (car (nameBindings state)) (car (valueBindings state))) (removeFromState var (cons (cdr (nameBindings state)) (cdr (valueBindings state))))))))) ;cons current to recurse into rest of list
(define nextVariable
  (lambda (state)
    (list (cdr (nameBindings state)) (cdr (valueBindings)))))
