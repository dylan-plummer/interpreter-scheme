;Dylan Plummer, Michael Tucci, Kevin Szmyd
;Interpreter part 1

(require "simpleParser.scm")

;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename)
    (run (parser filename) stateEmpty)))

;run evaluates the current parsetree statement and recursively runs the
;next line, returning the new state
(define run
  (lambda (parsetree state)
    (if (null? parsetree)
      state
      (run (nextLines parsetree) (stateGlobal (currentLine parsetree) state)))))

;run helpers
(define nextLines cdr)
(define currentLine car)

;stateGlobal takes a statement and a state and returns the new state after
;evaluating the statement
(define stateGlobal
  (lambda (statement state)
    (cond
      ((eq? (langValue statement) 'return) (returnValue (returnExp statement) state))
      ((eq? (langValue statement) 'while) (stateWhile (whileConditon statement) (whileBody statement) state))
      ((eq? (langValue statement) 'var) (stateDeclare (declareExp statement) state))
      ((eq? (langValue statement) '=) (stateAssign (assignExp statement)  state))
      ((eq? (langValue statement) 'if) (stateIf (ifCondition statement) (thenStatement statement) (elseStatement statement) state))
      (else (error "Incorrect syntax")))))

;global helpers
(define langValue car)
(define declareExp cdr)
(define returnExp cadr)
(define assignExp cdr)

;stateIf takes a condition, an if statement, an else statement, and a state
;and returns the new state after evaluating either of the statements depending on
;the condition
(define stateIf
  (lambda (condition statement else state)
    (if (mathValue condition state)
        (stateGlobal statement state)
        (stateGlobal else state))))

;if helpers
(define ifCondition cadr)
(define thenStatement caddr)
(define elseStatement
  (lambda (statement)
    (if (null? (cdddr statement))
        '()
        (cadddr statement))))

;stateDeclare takes a statement containing a variable and possibly an assignment
;and returns the new state with the variable declared
(define stateDeclare
  (lambda (statement state)
    (cond
      ((null? statement) state)
      ((null? (declareExp statement)) (addToState (variable statement) 'unassigned (removeFromState (variable statement) state)))
      (else (addToState (variable statement) (mathValue (assignmentExp statement) state) (removeFromState (variable statement) state))))))

;stateAssign takes a statement containing a variable and an expression and returns
;the new state with the variable assigned the value of the expression
(define stateAssign
  (lambda (statement state)
    (cond
      ((null? statement) state)
      (else (addToState (variable statement) (mathValue (assignmentExp statement) state) (removeFromState (variable statement) state))))))

;assignment helpers
(define assignmentExp cadr)
(define variable car)

;stateWhile takes a while loop condition, a loop body statement, and a state
;and returns the new state after the loop is executed
(define stateWhile
  (lambda (condition body state)
    (if (mathValue condition state)
      (stateWhile condition body (stateGlobal body state))
      state)))
;while helpers
(define whileConditon cadr)
(define whileBody caddr)

;returnValue takes an expression and a state and adds the value of the expression
;to the state as the variable 'return
(define returnValue
  (lambda (expression state)
    (cond
      ((number? (mathValue expression state)) (mathValue expression state))
      (else (boolValue (mathValue expression state))))))

;mathValue takes an expression and returns it's mathematical value (integer or boolean)
(define mathValue
  (lambda (exp state)
    (cond
      ;null/error checks
      ((null? exp) exp)
      ((number? exp) exp) ;no futher recursion needed, return number value
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((not (list? exp)) (searchState exp state)) ;not  number, yet not a list...must be a variable!
      ((number? (operator exp)) (error "Invalid expression")) ;the expression has no operator :(
      ((or (eq? (mathValue (operand1 exp) state) 'unassigned) (eq? (mathValue(operand2 exp) state) 'unassigned)) (error "Variable has not been assigned a value"))
      ;&&/||/! evaluation, needs to be in format (operator bool bool) else bad logic
      ((eq? '&& (operator exp)) (and (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '|| (operator exp)) (or (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? (operator exp) '!) (not (mathValue (operand1 exp) state)))
      ;boolean evaluation
      ((eq? '== (operator exp)) (= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '!= (operator exp)) (not (= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state))))
      ((eq? '<= (operator exp)) (>= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '>= (operator exp)) (>= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '<  (operator exp))  (< (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '>  (operator exp))  (> (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ;math evaluation
      ((and (eq? (operator exp) '-) (null? (binaryExp exp))) (- 0 (mathValue (operand1 exp) state)))
      ((eq? '+ (operator exp)) (+ (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '- (operator exp)) (- (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '* (operator exp)) (* (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '/ (operator exp)) (quotient (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '% (operator exp)) (remainder (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      (else (error "Unknown Operator")))))
;mathValue helpers
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define boolValue
  (lambda (bool)
    (if bool 'true 'false)))
(define binaryExp cddr)

;initial state, no variables declared or assigned, abstracted!
(define stateEmpty
  (list (list) (list)))

;bindings to the names of variables in the state
(define nameBindings car)
;bindings to the values of variables in the state
(define valueBindings cadr)

;addToState takes a variable and data and adds it to a state
(define addToState
  (lambda (var data state)
    (if (null? state)
      (concatNamesAndValues (list var) (list data))
      (concatNamesAndValues (addVarName var state) (addVarValue data state)))))

;addToState helpers
(define addVarName
  (lambda (var state)
    (cons var (nameBindings state))))
(define addVarValue
  (lambda (data state)
    (cons data (valueBindings state))))
(define concatNamesAndValues list)

;searchState takes a var and a state and returns associated data
(define searchState
  (lambda (var state)
    (cond
      ((or (null? (nameBindings state)) (null? (valueBindings state))) (error "Variable not declared"))
      ((eq? var (searchCurrentName state)) (searchCurrentValue state))
      (else (searchState var (concatNamesAndValues (searchNext (nameBindings state)) (searchNext (valueBindings state))))))))

;searchState helpers
(define searchNext cdr)
(define searchCurrentName caar)
(define searchCurrentValue caadr)

;removeFromState takes a var and removes it and returns the new state
(define removeFromState
  (lambda (var state)
    (cond
      ((null? var) state) ;null var, return given state
      ((null? state) state) ;null state, return stateEmpty
      ((or (null? (nameBindings state)) (null? (valueBindings state))) state) ;null names or values
      ((eq? var (removeCurrent (nameBindings state))) (list (removeRest (nameBindings state)) (removeRest (valueBindings state)))) ;var match, return everything else
      (else (concatNamesAndValues (cons (removeCurrent (nameBindings state)) (removeCurrent (removeFromState var (nextVariable state)))) (cons (removeCurrent (valueBindings state)) (removeNext (removeFromState var (nextVariable state)))))))))

;removeFromState helpers
(define nextVariable
  (lambda (state)
    (concatNamesAndValues (removeRest (nameBindings state)) (removeRest (valueBindings state)))))
(define removeCurrent car)
(define removeNext cadr)
(define removeRest cdr)
