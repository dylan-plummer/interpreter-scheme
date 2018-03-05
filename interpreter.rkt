;Dylan Plummer, Michael Tucci, Kevin Szmyd
;Interpreter part 1

(require "simpleParser.scm")

;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename)
    (run (parser filename) (list stateEmpty) (defaultCont) (defaultCont))))

;run evaluates the current parsetree statement and recursively runs the
;next line, returning the new state
(define run
  (lambda (parsetree state return continue)
    (display "block ")(display state) (newline)
    (if (null? parsetree)
      state
      (run (nextLines parsetree) (stateGlobal (currentLine parsetree) state return continue) return continue))))

;run helpers
(define nextLines cdr)
(define currentLine car)

;stateGlobal takes a statement and a state and returns the new state after
;evaluating the statement
(define stateGlobal
  (lambda (statement state return continue)
    (display statement) (newline)
    (display "current ") (display state) (newline)
    (cond
      ((null? statement) state)
      ((eq? (langValue statement) 'begin) (stateBeginBlock (cdr statement) state return continue))
      ((eq? (langValue statement) 'return) (returnValue (returnExp statement) state))
      ((eq? (langValue statement) 'while) (stateWhile (whileConditon statement) (whileBody statement) state return continue))
      ((eq? (langValue statement) 'var) (stateDeclare (declareExp statement) state))
      ((eq? (langValue statement) '=) (stateAssign (assignExp statement)  state))
      ((eq? (langValue statement) 'if) (stateIf (ifCondition statement) (thenStatement statement) (elseStatement statement) state return continue))
      (else (error "Incorrect syntax")))))

;global helpers
(define langValue car)
(define declareExp cdr)
(define returnExp cadr)
(define assignExp cdr)

(define stateBeginBlock
  (lambda (expression state return continue)
    (display "block ")(display expression)(display " state ") (display state) (newline)
    (stateEndBlock (runBlock expression (cons stateEmpty state) return continue))))
(define runBlock
  (lambda (block state return continue)
    (if (null? block)
      state
      (runBlock (nextLines block) (stateGlobal (car block) state return continue) return continue))))
;gets rid of the layer
(define stateEndBlock
  (lambda (state)
    (display "end block")(display state) (newline)
    state))

;stateIf takes a condition, an if statement, an else statement, and a state
;and returns the new state after evaluating either of the statements depending on
;the condition
(define stateIf
  (lambda (condition statement else state return continue)
    (if (mathValue condition state)
        (stateGlobal statement state return continue)
        (stateGlobal else state return continue))))

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
      (else (replaceInState (variable statement) (mathValue (assignmentExp statement) state) state)))))
      ;(else (addToState (variable statement) (mathValue (assignmentExp statement) state) (removeFromState (variable statement) state))))))

;assignment helpers
(define assignmentExp cadr)
(define variable car)

;stateWhile takes a while loop condition, a loop body statement, and a state
;and returns the new state after the loop is executed
(define stateWhile
  (lambda (condition body state return continue)
    (if (mathValue condition state)
      (stateWhile condition body (stateGlobal body state return continue) return continue)
      state)))
;while helpers
(define whileConditon cadr)
(define whileBody caddr)

;returnValue takes an expression and a state and returns the value of the expression
;as an integer or a boolean
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
      ((eq? '!= (operator exp)) (not (= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state))))
      ((eq? '! (operator exp)) (mathValue (operand1 exp) state))
      ((and (eq? (operator exp) '-) (null? (binaryExp exp))) (- 0 (mathValue (operand1 exp) state)))
      ((or (eq? (mathValue (operand1 exp) state) 'unassigned) (eq? (mathValue(operand2 exp) state) 'unassigned)) (error "Variable has not been assigned a value"))
      ;&&/||/! evaluation, needs to be in format (operator bool bool) else bad logic
      ((eq? '&& (operator exp)) (and (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '|| (operator exp)) (or (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? (operator exp) '!) (not (mathValue (operand1 exp) state)))
      ;boolean evaluation
      ((eq? '== (operator exp)) (= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '<= (operator exp)) (<= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '>= (operator exp)) (>= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '<  (operator exp))  (< (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '>  (operator exp))  (> (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ;math evaluation
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
(define nameBindings
  (lambda (state)
    ;(display "names ")(display state) (newline)
    (car (firstLayer state))))
;bindings to the values of variables in the state
(define valueBindings
    (lambda (state)
      (cadr (firstLayer state))))
(define firstLayer car)
(define nextLayer cdr)

;addToState takes a variable and data and adds it to a state
(define addToState
  (lambda (var data state)
    (display "add ") (display state) (newline) 
    (if (null? state)
      (cons (concatNamesAndValues (list var) (list data)) (nextLayer state))
      (cons (concatNamesAndValues (addVarName var state) (addVarValue data state)) (nextLayer state)))))

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
    (display "search ") (display var)(display " in ")(display state) (newline) 
    (cond
      ((or (null? (nameBindings state)) (null? (valueBindings state))) (searchState var (nextLayer state)))
      ((eq? var (searchCurrentName state)) (searchCurrentValue state))
      ((null? (cdr (nameBindings state))) (searchState var (nextLayer state)))
      (else (searchState var (cons (concatNamesAndValues (searchNext (nameBindings state)) (searchNext (valueBindings state))) (nextLayer state)))))))

;searchState helpers
(define searchNext cdr)
(define searchCurrentName
  (lambda (state)
    (caar (firstLayer state))))
(define searchCurrentValue
  (lambda (state)
    (caadr (firstLayer state))))

(define replaceInState*
  (lambda (var val state)
    (display "replace ")(display var)(display " in ") (display state) (newline) 
    (cond
      ((or (null? (nameBindings state)) (null? (valueBindings state))) (replaceInState var val (nextLayer state)))
      ((eq? var (searchCurrentName state)) (cons (list (nameBindings state) (cons val (cdr (valueBindings state)))) (nextLayer state)))
      ((null? (cdr (nameBindings state))) (replaceInState var val (nextLayer state)))
      (else (addToState (car (nameBindings state)) (car (valueBindings state)) (replaceInState var val (cons (list (cdr (nameBindings state)) (cdr (valueBindings state))) (nextLayer state))))))))
      ;(else (cons (firstLayer state) (replaceInState var val (nextLayer state)))))))
(define replaceInState
  (lambda (var val state)
    (display "replace ")(display var)(display " in ") (display state) (newline)
    (if (null? (nextLayer state))
        (list (replaceInLayer var val (firstLayer state)))
        (cons (replaceInLayer var val (firstLayer state)) (replaceInState var val (nextLayer state))))))

; replace-value
; Given a variable name, value, and state, find the location within the state where the given variable name is stored and replace its value, and return the new state
(define replaceInLayer
  (lambda (var val layer)
    (display "replaceLayer ")(display var)(display " in ") (display layer) (newline)
    (replaceInLayer-cps var val (car layer) (cadr layer) (lambda (l1 l2) (list l1 l2)))))

; tail recursive helper for replace-value
(define replaceInLayer-cps
  (lambda (var val names values return)
    (display "replaceLayer-cps ")(display var)(display " in ") (display names)(display values) (newline)
    (cond
      ((null? names) (return names values))
      ((equal? var (car names)) (return names (cons val (cdr values))))
      (else (replaceInLayer-cps var val (cdr names) (cdr values) (lambda (l1 l2) (return (cons (car names) l1) (cons (car values) l2))))))))


;removeFromState takes a var and removes it and returns the new state
(define removeFromState
  (lambda (var state)
    (if (null? (nextLayer state))
        (list (removeFromLayer var (firstLayer state)))
        (cons (removeFromLayer var (firstLayer state)) (nextLayer state)))))
(define removeFromLayer
  (lambda (var layer)
    (cond
      ((null? var) layer)
      ((null? layer) layer)
      ((or (null? (car layer)) (null? (cadr layer))) layer)
      ((eq? var (car (car layer))) (list (cdr (car layer)) (cdr (cadr layer))))
      (else (list (cons (car (car layer)) (car (removeFromLayer var (nextVar layer)))) (cons (car (cadr layer)) (cadr (removeFromLayer var (nextVar layer)))))))))
(define nextVar
  (lambda (layer)
    (list (cdr (car layer)) (cdr (cadr layer)))))

(define defaultCont
  (lambda ()
    (lambda (v) v)))