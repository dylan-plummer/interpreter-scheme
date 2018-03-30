;Dylan Plummer, Michael Tucci, Kevin Szmyd
;Interpreter part 1
(require "functionParser.scm")
(require rackunit)


;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (stateGlobal (parser filename) (list stateEmpty) return defaultContinue defaultBreak defaultThrow)))))

;run evaluates the current parsetree statement and recursively runs the
;next line, returning the new state
(define run
  (lambda (parsetree state return continue break throw)
    (if (null? parsetree)
      state
      (run (nextLines parsetree) (evaluateStatement (currentLine parsetree) state return continue break throw) return continue break throw))))

;stateGlobal takes in the parse tree and creates the base layer of the state
;which contains only functions and global variables
(define stateGlobal
  (lambda (parsetree state return continue break throw)
    (if (null? parsetree)
        (runFunction (searchState 'main state) '() state return continue break throw)
        (stateGlobal (nextLines parsetree) (evaluateGlobalStatement (currentLine parsetree) state return continue break throw) return continue break throw))))
;evaluateGlobalStatement takes a statement from the outermost layer of the program
;and either binds a function to its closure or adds a variable to the state
(define evaluateGlobalStatement
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state)
      ((eq? (langValue statement) 'function) (bindFunctionClosure statement state))
      ((eq? (langValue statement) 'var) (stateDeclare (declareExp statement) state return continue break throw))
      (else (error "Incorrect Syntax")))))

;bindFunctionClosure takes a statement of the form (function <name> <params> <body>)
;and returns the new state resulting from adding the function to the current state
(define bindFunctionClosure
  (lambda (statement state)
    (addToState (functionName statement) (createClosure (functionParams statement) (functionBody statement) (functionName statement)) state)))
(define functionName cadr)
(define functionParams caddr)
(define functionBody cadddr)

(define createClosure
  (lambda (params body name)
    (list params body (generateFunctionState name))))
(define generateFunctionState
  (lambda (name)
    (lambda (state)
      (cons stateEmpty state))))
(define runFunction
  (lambda (closure params state return continue break throw)
    ;(logln "runFunction state" state) 
    (statePopLayer (runBlock (getFunctionBody closure)  (setActualParams params (getFormalParams closure) ((getStateFunc closure) state)  return continue break throw) return continue break throw))))
(define getFormalParams car)
(define getStateFunc caddr)
(define getFunctionBody cadr)
(define setActualParams
  (lambda (actualParams formalParams state return continue break throw)
    ;(logln "setActualParams" actualParams)
    (if (null? actualParams)
      state
      (setActualParams (cdr actualParams) (cdr formalParams) (addToState (car formalParams) (mathValue (car actualParams) (nextLayers state) return continue break throw) state) return continue break throw))))

;run helpers
(define nextLines cdr)
(define currentLine car)

;evaluateStatement takes a statement and a state and returns the new state after
;evaluating the statement
(define evaluateStatement
  (lambda (statement state return continue break throw)
    ;(logln "evaluateState state" state)
    (cond
      ((null? statement) state)
      ((eq? (langValue statement) 'begin) (statePopLayer (stateBeginBlock (wholeBody statement) state return continue break throw)))
      ((eq? (langValue statement) 'break) (break (statePopLayer state)))
      ((eq? (langValue statement) 'continue) (stateContinue state continue))
      ((eq? (langValue statement) 'try)  (stateTry statement state return continue break throw))
      ((eq? (langValue statement) 'throw) (throw (mathValue (throwStatement statement) state return continue break throw) state))
      ((eq? (langValue statement) 'finally) (runBlock (blockBody statement) state return continue break throw)) ;executes code within regardless of rest of the try block, has try block scope
      ((eq? (langValue statement) 'return) (returnValue (returnExp statement) state return continue break throw))
      ((eq? (langValue statement) 'while) (stateWhile (whileConditon statement) (whileBody statement) state return continue break throw))
      ((eq? (langValue statement) 'var) (stateDeclare (declareExp statement) state return continue break throw))
      ((eq? (langValue statement) '=) (stateAssign (assignExp statement) state return continue break throw))
      ((eq? (langValue statement) 'if) (stateIf (ifCondition statement) (thenStatement statement) (elseStatement statement) state return continue break throw))
      (else (error "Incorrect syntax")))))

;evaluateStatement helpers
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
    ;(logln "Begin block state" state)
    (runBlock expression (cons stateEmpty state) return continue break throw)))

(define runBlock
  (lambda (block state return continue break throw)
    ;(logln "runBlock" state)
    (if (null? block)
      state
      (runBlock (nextLines block) (evaluateStatement (currentLine block) state return continue break throw) return continue break throw))))
;gets rid of the first layer in state
(define statePopLayer
  (lambda (state)
    ;(logln "pop layer" state)
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
    (if (mathValue condition state return continue break throw)
        (evaluateStatement statement state return continue break throw)
        (evaluateStatement else state return continue break throw))))

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
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state)
      ((null? (declareExp statement)) (addToState (variable statement) 'unassigned (removeFromState (variable statement) state)))
      (else (addToState (variable statement) (mathValue (assignmentExp statement) state return continue break throw) (removeFromState (variable statement) state))))))

;stateAssign takes a statement containing a variable and an expression and returns
;the new state with the variable assigned the value of the expression
(define stateAssign
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state)
      (else (replaceInState (variable statement) (mathValue (assignmentExp statement) state return continue break throw) state)))))

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
    (if (mathValue condition state return continue break throw)
        (stateWhileLoop condition body (call/cc
                                        (lambda (newContinue)
                                          (evaluateStatement body state return newContinue break throw)))
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
      ((number? (mathValue expression state return continue break throw)) (return (mathValue expression state  return continue break throw)))
      (else (return (boolValue (mathValue expression state  return continue break throw)))))))


;mathValue takes an expression and returns it's mathematical value (integer or boolean)
(define mathValue
  (lambda (exp state return continue break throw)
    ;(logln "mathValue exp" exp)
    (cond
      ;null/error checks
      ((null? exp) exp)
      ((number? exp) exp) ;no futher recursion needed, return number value
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((not (list? exp)) (searchState exp state)) ;not  number, yet not a list...must be a variable!
      ((number? (operator exp)) (error "Invalid expression")) ;the expression has no operator :(
      ((eq? '!= (operator exp)) (not (= (mathValue (operand1 exp) state) (mathValue (operand2 exp) state) return continue break throw)))
      ((eq? '! (operator exp)) (not (mathValue (operand1 exp) state return continue break throw)))
      ((and (eq? (operator exp) '-) (null? (binaryExp exp))) (- 0 (mathValue (operand1 exp) state)))
      ((eq? (operator exp) 'funcall)
       (call/cc
        (lambda (newReturn)
          (runFunction (searchState (cadr exp) state) (getActualParams exp) state newReturn continue break throw))))
      ((or (eq? (mathValue (operand1 exp) state return continue break throw) 'unassigned) (eq? (mathValue(operand2 exp) state return continue break throw) 'unassigned)) (error "Variable has not been assigned a value"))
      ;&&/||/! evaluation, needs to be in format (operator bool bool) else bad logic
      ((eq? '&& (operator exp)) (and (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '|| (operator exp)) (or (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ;boolean evaluation
      ((eq? '== (operator exp)) (equal? (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '<= (operator exp)) (<= (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '>= (operator exp)) (>= (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '<  (operator exp))  (< (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '>  (operator exp))  (> (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ;math evaluation
      ((eq? '+ (operator exp)) (+ (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '- (operator exp)) (- (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '* (operator exp)) (* (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '/ (operator exp)) (quotient (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      ((eq? '% (operator exp)) (remainder (mathValue (operand1 exp) state return continue break throw) (mathValue (operand2 exp) state return continue break throw)))
      (else (error "Unknown Operator")))))

;mathValue helpers
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define getActualParams
  (lambda (exp)
    ;(logln "getActualParams exp" exp)
    (cddr exp)))

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

;searchState takes a named and a state and returns associated data or procedure
(define searchState
  (lambda (var state)
    ;(display "search ") (display var)(display " in ")(display state) (newline)
    (cond
      ((null? state) (error "Variable/Function not in scope"))
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

(define logln
  (lambda (title val)
    (display title)(display ":")(display val)(newline)))

(check-equal? (interpret "tests3/test1") 10 "Test 1")
(check-equal? (interpret "tests3/test2") 14 "Test 2")
(check-equal? (interpret "tests3/test3") 45 "Test 3")
;(check-equal? (interpret "tests3/test4") 55 "Test 4")
(check-equal? (interpret "tests3/test5") 1 "Test 5")
(check-equal? (interpret "tests3/test6") 115 "Test 6")
(check-equal? (interpret "tests3/test7") 'true "Test 7")
(check-equal? (interpret "tests3/test8") 20 "Test 8")
(check-equal? (interpret "tests3/test9") 24 "Test 9")
(check-equal? (interpret "tests3/test10") 2 "Test 10")
(check-equal? (interpret "tests3/test11") 35 "Test 11")
(check-equal? (interpret "tests3/test12") error "Test 12")
(check-equal? (interpret "tests3/test13") 90 "Test 13")
(check-equal? (interpret "tests3/test14") 69 "Test 14")
(check-equal? (interpret "tests3/test15") 87 "Test 15")
(check-equal? (interpret "tests3/test16") 64 "Test 16")
(check-equal? (interpret "tests3/test17") error "Test 17")
(check-equal? (interpret "tests3/test18") 125 "Test 18")
(check-equal? (interpret "tests3/test19") 100 "Test 19")
(check-equal? (interpret "tests3/test20") 2000400 "Test 20")