;Dylan Plummer, Michael Tucci, Kevin Szmyd
;Interpreter part 1
(require "simpleParser.scm")

;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (run (parser filename) (list stateEmpty) return defaultContinue defaultBreak defaultThrow)))))

;run evaluates the current parsetree statement and recursively runs the
;next line, returning the new state
(define run
  (lambda (parsetree state return continue break throw)
    (if (null? parsetree)
      state
      (run (nextLines parsetree) (stateGlobal (currentLine parsetree) state return continue break throw) return continue break throw))))

;run helpers
(define nextLines cdr)
(define currentLine car)

;stateGlobal takes a statement and a state and returns the new state after
;evaluating the statement
(define stateGlobal
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state)
      ((eq? (langValue statement) 'begin) (statePopLayer (stateBeginBlock (wholeBody statement) state return continue break throw)))
      ((eq? (langValue statement) 'break) (break (statePopLayer state)))
      ((eq? (langValue statement) 'continue) (stateContinue state continue))
      ((eq? (langValue statement) 'try)  (stateTry statement state return continue break throw))
      ((eq? (langValue statement) 'throw) (throw (mathValue (throwStatement statement) state) state))
      ((eq? (langValue statement) 'finally) (runBlock (blockBody statement) state return continue break throw)) ;executes code within regardless of rest of the try block, has try block scope
      ((eq? (langValue statement) 'return) (returnValue (returnExp statement) state return continue break throw))
      ((eq? (langValue statement) 'while) (stateWhile (whileConditon statement) (whileBody statement) state return continue break throw))
      ((eq? (langValue statement) 'var) (stateDeclare (declareExp statement) state))
      ((eq? (langValue statement) '=) (stateAssign (assignExp statement)  state))
      ((eq? (langValue statement) 'if) (stateIf (ifCondition statement) (thenStatement statement) (elseStatement statement) state return continue break throw))
      (else (error "Incorrect syntax")))))

;stateGlobal helpers
(define langValue car)
(define declareExp cdr)
(define returnExp cadr)
(define assignExp cdr)
(define blockBody cadr)
(define throwStatement cadr)

(define catchName (lambda (statement) (car (cadr statement))))
(define wholeBody cdr)


(define stateBeginBlock
  (lambda (expression state return continue break throw)
    (runBlock expression (cons stateEmpty state) return continue break throw)))

(define runBlock
  (lambda (block state return continue break throw)
    (if (null? block)
      state
      (runBlock (nextLines block) (stateGlobal (currentLine block) state return continue break throw) return continue break throw))))
;gets rid of the first layer in state
(define statePopLayer
  (lambda (state)
    (cdr state)))

(define stateTry
  (lambda (statement state return continue break throw)
    (cond
      ((and (null? (catchBody statement)) (null? (finallyBlock statement))) (tryCatchFinally (blockBody statement) NULL NULL state return continue break throw)) ;no catch, no finally
      ((null? (catchBody statement)) (tryCatchFinally (blockBody statement) NULL (finallyBlock statement) state return continue break throw)) ;no catch, try, finally
      ((null? (finallyBlock statement)) (tryCatchFinally (blockBody statement) (catchBody statement) NULL state return continue break throw)) ;no finally, try, catch
      (else (tryCatchFinally (blockBody statement) (catchBody statement) (finallyBlock statement) state return continue break throw)))))

(define finallyBlock cdddr)
(define catchBody caddr)

(define NULL (list))

(define tryCatchFinally
  (lambda (tryBody catchBody finallyBody state return continue break throw)
    (cond
      ((null? finallyBody)
       (call/cc
        (lambda (newThrow)
          (run tryBody state return continue break (lambda (val newState) (newThrow (stateCatch catchBody val newState return continue break throw)))))))
      (else (runBlock finallyBody
                       (call/cc
                        (lambda (newThrow)
                          (run tryBody state return continue break (lambda (val newState) (newThrow (stateCatch catchBody val newState return continue break throw)))))) return continue break throw)))))

(define stateCatch ;creates a new block and scope for catch body with the thrown variable
  (lambda (body val state return continue break throw)
    (if (null? body)
      state
      (runBlock (catchBody body) (addToState (catchValue body) val state) return continue break throw))))
(define catchValue caadr)

(define stateContinue
  (lambda (state continue)
    (continue (statePopLayer state))))

;stateIf takes a condition, an if statement, an else statement, and a state
;and returns the new state after evaluating either of the statements depending on
;the condition
(define stateIf
  (lambda (condition statement else state return continue break throw)
    (if (mathValue condition state)
        (stateGlobal statement state return continue break throw)
        (stateGlobal else state return continue break throw))))

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

;assignment helpers
(define assignmentExp cadr)
(define variable car)

;stateWhile takes a while loop condition, a loop body statement, and a state
;and returns the new state after the loop is executed
(define stateWhile
  (lambda (condition body state return continue break throw)
    (call/cc
     (lambda (newBreak)
       (stateWhileLoop condition body state return continue newBreak throw)))))

(define stateWhileLoop
  (lambda (condition body state return continue break throw)
    (if (mathValue condition state)
        (stateWhileLoop condition body (call/cc
                                        (lambda (newContinue)
                                          (stateGlobal body state return newContinue break throw)))
                        return continue break throw)
        state)))

;while helpers
(define whileConditon cadr)
(define whileBody caddr)

;returnValue takes an expression and a state and returns the value of the expression
;as an integer or a boolean
(define returnValue
  (lambda (expression state return continue break throw)
    (cond
      ((number? (mathValue expression state)) (return (mathValue expression state)))
      (else (return (boolValue (mathValue expression state)))))))


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
      ((eq? '! (operator exp)) (not (mathValue (operand1 exp) state)))
      ((and (eq? (operator exp) '-) (null? (binaryExp exp))) (- 0 (mathValue (operand1 exp) state)))
      ((or (eq? (mathValue (operand1 exp) state) 'unassigned) (eq? (mathValue(operand2 exp) state) 'unassigned)) (error "Variable has not been assigned a value"))
      ;&&/||/! evaluation, needs to be in format (operator bool bool) else bad logic
      ((eq? '&& (operator exp)) (and (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
      ((eq? '|| (operator exp)) (or (mathValue (operand1 exp) state) (mathValue (operand2 exp) state)))
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
(define nextLayers cdr)

;addToState takes a variable and data and adds it to a state
(define addToState
  (lambda (var data state)
    ;(display "add ") (display state) (newline)
    (if (null? state)
      (cons (concatNamesAndValues (list var) (list data)) (nextLayers state))
      (cons (concatNamesAndValues (addVarName var state) (addVarValue data state)) (nextLayers state)))))

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
    ;(display "search ") (display var)(display " in ")(display state) (newline)
    (cond
      ((null? state) (error "Variable not in scope"))
      ((or (null? (nameBindings state)) (null? (valueBindings state))) (searchState var (nextLayers state)))
      ((eq? var (searchCurrentName state)) (searchCurrentValue state))
      ((null? (restOfNames (nameBindings state))) (searchState var (nextLayers state)))
      (else (searchState var (cons (concatNamesAndValues (searchNext (nameBindings state)) (searchNext (valueBindings state))) (nextLayers state)))))))

;searchState abstraction
(define restOfNames cdr)

(define inLayer?
  (lambda (var layer)
    (cond
      ((null? layer) #f)
      ((null? (car layer)) #f)
      ((eq? (caar layer) var) #t)
      (else (inLayer? var (list (cdr (car layer)) (cdr (cdr layer))))))))

;searchState helpers
(define searchNext cdr)

(define searchCurrentName
  (lambda (state)
    (caar (firstLayer state))))

(define searchCurrentValue
  (lambda (state)
    (caadr (firstLayer state))))

;replaceInstate takes a variable, a value, and a state and returns the new state
;with that variable's value replaced with the given value
(define replaceInState
  (lambda (var val state)
    ;recurse through layer, if it's empty, go to the next layer
    ;if variable found, replace it, otherwise keep going through layer
    (cond
      ((inLayer? var (firstLayer state)) (cons (replaceInLayer var val (firstLayer state)) (nextLayers state)))
      (else (cons (firstLayer state) (replaceInState var val (nextLayers state)))))))

; replaceInLayer
; Given a variable name, value, and layer, find the location within the layer
; where the given variable name is stored and replace its value, and return the new layer
(define replaceInLayer
  (lambda (var val layer)
    ;(display "replaceLayer ")(display var)(display " in ") (display layer) (newline)
    (replaceInLayer-cps var val (car layer) (cadr layer) (lambda (l1 l2) (list l1 l2)))))

; tail recursive helper for replaceInLayer
(define replaceInLayer-cps
  (lambda (var val names values return)
    ;(display "replaceLayer-cps ")(display var)(display " in ") (display names)(display values) (newline)
    (cond
      ((null? names) (return names values))
      ((equal? var (car names)) (return names (cons val (cdr values))))
      (else (replaceInLayer-cps var val (cdr names) (cdr values) (lambda (l1 l2) (return (cons (car names) l1) (cons (car values) l2))))))))

;helper functions for replacing in layers


;removeFromState takes a var and removes it and returns the new state
(define removeFromState
  (lambda (var state)
    (if (null? (nextLayers state))
        (list (removeFromLayer var (firstLayer state)))
        (cons (removeFromLayer var (firstLayer state)) (nextLayers state)))))

(define removeFromLayer
  (lambda (var layer)
    (cond
      ((null? var) layer)
      ((null? layer) layer)
      ((or (null? (firstBlock layer)) (null? (secondBlock layer))) layer)
      ((eq? var (firstVar (firstBlock layer))) (list (remainingVars (firstBlock layer)) (remainingVars (secondBlock layer))))
      (else (list (cons (firstVar (firstBlock layer)) (firstVar (removeFromLayer var (nextVar layer)))) (cons (firstVar (secondBlock layer)) (secondBlock (removeFromLayer var (nextVar layer)))))))))

;helper functions for layering
(define firstBlock car)
(define secondBlock cadr)
(define firstVar car)
(define remainingVars cdr)

(define nextVar
  (lambda (layer)
    (list (remainingVars (firstBlock layer)) (remainingVars (secondBlock layer)))))

(define defaultContinue
  (lambda (state)
    (error "Can't call continue here")))

(define defaultBreak
  (lambda (state)
    (error "Can't call break here")))

(define defaultThrow
  (lambda (val state)
    (error "Thrown statement not caught")))