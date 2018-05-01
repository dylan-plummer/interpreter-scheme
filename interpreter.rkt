;Dylan Plummer, Michael Tucci, Kevin Szmyd
;Interpreter part 4

(require "classParser.scm")
;(require rackunit) ;uncomment to run unit tests


;interpret takes a filename, calls the parser on that file,
;evaluates the parse tree returned by the parser, and returns
;the proper value
(define interpret
  (lambda (filename classname)
    (call/cc
     (lambda (return)
       (let ((environment (stateGlobal (parser filename) (list stateEmpty) return defaultContinue defaultBreak defaultThrow)))
         (runMainInClass (searchState (string->symbol classname) environment) (string->symbol classname) environment return defaultContinue defaultBreak defaultThrow))))))


; returnFunctionValue takes(lambda (closure params state return continue break throw type)
(define runMainInClass
  (lambda (classClosure className state return continue break throw)
    (call/cc
     (lambda (newReturn)
       (set-box! mainClass className)
       (returnFunctionValue (searchState 'main (list (classMethods classClosure))) NULL state newReturn continue break throw (searchState className state))))))

(define classMethods car)

;run evaluates the current parsetree statement and recursively runs the
;next line, returning the new state
(define run
  (lambda (parsetree type state return continue break throw)
    (call/cc
     (lambda (newReturn)
       (set-box! newState state)
       (if (null? parsetree)
           state
           (run (nextLines parsetree) type (evaluateStatement (currentLine parsetree) state type newReturn continue break throw) return continue break throw))))))

(define stateGlobal
  (lambda (parsetree state return continue break throw)
    (if (null? parsetree)
      state
      (stateGlobal (nextLines parsetree) (evaluateGlobalStatement (currentLine parsetree) state return continue break throw) return continue break throw))))

(define evaluateGlobalStatement
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state)
      ((and (eq? (langValue statement) 'class) (null? (caddr statement)) (addToState (className statement) (createClassClosure (className statement) NULL (classBody statement) state) state)))
      ((and (eq? (langValue statement) 'class) (eq? (caaddr statement) 'extends)) (addToState (className statement) (createClassClosure (className statement) (searchState (superClass statement) state) (classBody statement) state) state))
      (else state))))

(define className cadr)
(define superClass
  (lambda (exp)
    (cadr (caddr exp))))
(define classBody cadddr)

;a class closure stores the methods of the class and its inherited methods,
;the fields (static field names and values, instance field names),
;the superclass (closure?)
;the constructors (not inherited)
(define createClassClosure
  (lambda (name super body state)
    (cond
      ((null? super) (list (getClassMethods name body stateEmpty) (getClassFields body stateEmpty) NULL)) ;create new class
      (else (list (mergeLayers (car super) (getClassMethods name body stateEmpty)) (mergeLayers (cadr super) (getClassFields body stateEmpty)) super))))) ;class extends

(define getClassFields
  (lambda (body state)
    (cond
      ((null? body) state)
      ((null? (cdr (car body))) (getClassFields (cdr body) state))
      ((and (eq? (caar body) 'var) (null? (cddr (car body)))) (getClassFields (cdr body) (addToLayer (cadr (car body)) 'unassigned state)))
      ((eq? (caar body) 'var) (getClassFields (cdr body) (addToLayer (cadr (car body)) (caddr (car body)) state)))
      (else (getClassFields (cdr body) state)))))


(define getClassMethods
  (lambda (name body state)
    (cond
      ((null? body) state)
      ((eq? (caar body) 'static-function) (getClassMethods name (cdr body) (addToLayer (cadr (car body)) (createFunctionClosure (functionParams (car body)) (functionBody (car body)) (functionName (car body)) name) state)))
      ((eq? (caar body) 'function) (getClassMethods name (cdr body) (addToLayer (cadr (car body)) (createFunctionClosure (cons 'this (functionParams (car body))) (functionBody (car body)) (functionName (car body)) name) state)))
      (else (getClassMethods name (cdr body) state)))))


;an instance closure contains the class (closure) and the instance
;field values (in reverse order)
;reverse the field values and
(define createInstanceClosure
  (lambda (typeName state)
    (list (searchState typeName state) (getFieldValues typeName state))))

(define getFieldValues
  (lambda (typeName state)
    (cadr (searchState typeName state))))

;stateClass takes in the parse tree and creates the base layer of the class state
;which contains only functions and class fields
(define stateClass
  (lambda (parsetree state return continue break throw)
    (call/cc
     (lambda (newReturn)
       (if (null? parsetree)
           state
           (stateClass (nextLines parsetree) (evaluateClassStatement (currentLine parsetree) state newReturn continue break throw type) newReturn continue break throw))))))

;evaluateClassStatement takes a statement from the outermost layer of the program
;and either binds a function to its closure or adds a variable to the state
(define evaluateClassStatement
  (lambda (statement state return continue break throw type)
    (set-box! newState state)
    (cond
      ((null? statement) state)
      ((eq? (langValue statement) 'function) (bindFunctionClosure statement state))
      ((eq? (langValue statement) 'var) (stateDeclare (declareExp statement) state return continue break throw type))
      ((eq? (langValue statement) '=) (stateAssign (assignExp statement) state return continue break throw))
      (else state))))

;bindFunctionClosure takes a statement of the form (function <name> <params> <body>)
;and returns the new state resulting from adding the function to the current state
(define bindFunctionClosure
  (lambda (statement classname state)
    (addToState (functionName statement) (createFunctionClosure (cons 'this (functionParams statement)) (functionBody statement) (functionName statement) classname) state)))

;createFunctionClosure creates a list that contains a function's formal parameters,
;body, and a function that generates the state to run the function in
(define createFunctionClosure
  (lambda (params body name classname)
    (list params body (generateFunctionState name) (generateFunctionClass classname))))

;generateFunctionState takes the name of a function and generates a function
;that takes a state and returns the state where the scope of the function
;is defined
(define generateFunctionState
  (lambda (name)
    (lambda (state)
      (cond
        ((null? (nextLayers state)) state)
        ((inLayer? name (firstLayer (nextLayers state))) state)
        ((inLayer? 'this (firstLayer state)) state)
        (else ((generateFunctionState name) (nextLayers state)))))))

(define generateFunctionClass
  (lambda (classname)
    (lambda (state)
      (searchState classname state))))

;evalFunction and runFunction evaluate a function and upon reaching the return continuation returns
;the state instead of the returned value
(define evalFunction
  (lambda (closure params state return continue break throw type)
    (runFunction (getFunctionBody closure) (cons (setVariablesInScope state (setActualParams params (getFormalParams closure) state (cons stateEmpty state) return continue break throw type) type) ((getStateFunc closure) state)) return continue break throw type)))
(define runFunction
  (lambda (parsetree state return continue break throw type)
    (if (null? parsetree)
        state
        (runFunction (nextLines parsetree) (evaluateStatement (currentLine parsetree) state type (functionStateReturn state) continue break throw) return continue break throw type))))

(define getFunctionType cadddr)

;functionStateReturn replaces the return continuation when evaluating a function as a
;standalone statement
(define functionStateReturn
  (lambda (state)
    (lambda (v)
      state)))

;returnFunctionValue executes a function and returns its value
(define returnFunctionValue
  (lambda (closure params state return continue break throw type)
    (run (getFunctionBody closure) type (cons (setVariablesInScope state (setActualParams params (getFormalParams closure) state (cons stateEmpty state) return continue break throw type) type) ((getStateFunc closure) state)) return continue break throw)))

(define returnDotFunctionValue
  (lambda (exp params state return continue break throw type)
    (cond
      ((eq? (cadr exp) 'super) (let ((closure (searchState (caddr exp) (getDotInstance (cadr exp) state)))) (returnFunctionValue closure params state return continue break throw (caddr type))))
      ((and (list? (cadr exp)) (eq? (car (cadr exp)) 'new)) (returnFunctionValue (searchState (caddr exp) (car (createInstanceClosure (cadr (cadr exp)) state))) params state return continue break throw type))
      ((inState? 'super state) (returnFunctionValue (searchState (caddr exp) type) params state return continue break throw (caddr type)))
      (else (returnFunctionValue (searchState (caddr exp) (recurseIntoState type)) params state return continue break throw type)))))

(define evalDotFunction
  (lambda (exp params state return continue break throw type)
    (let ((updatedState (evalFunction (searchState (caddr exp) (recurseIntoState (searchForFieldValue (cadr exp) type state))) params state return continue break throw type)))
      (if (eq? (cadr exp) 'super) (replaceInState (cadr exp) (searchState 'this updatedState) (replaceInState 'this (searchState 'this updatedState) state)) 
          (replaceInState (cadr exp) (searchState 'this updatedState) state)))))

(define onionCount
  (lambda (acc state)
    (if (list? (car state)) (onionCount (+ 1 acc) (car state))
        acc)))
(define recurseIntoState
  (lambda (state)
    (let ((count (onionCount 0 state)))
      (if (= count 2) state
          (recurseIntoState (car state))))))

(define dotGetFieldValue
  (lambda (exp state)
    (cond
      ((and (list? (cadr exp)) (eq? (car (cadr exp)) 'new)) (searchState (caddr exp) (list (cadr (createInstanceClosure (cadr (cadr exp)) state)))))
      (else (searchState (caddr exp) (list (cadr (getDotInstance (cadr exp) state))))))))

(define setFieldValue
  (lambda (instance val name)
    (cons (list (car instance)) (replaceInState name val (list (cadr instance))))))
   


(define getDotInstance
  (lambda (instanceName state)
    (searchState instanceName state)))


;setActualParams takes the function arguments and parameters and returns a layer
;containing the parameters and their assigned values
(define setActualParams
  (lambda (actualParams formalParams state funcState return continue break throw type)
    (cond
      ((> (length actualParams) (length formalParams)) (error "Too many arguments!"))
      ((< (length actualParams) (length formalParams)) (error "Not enough arguments!"))
      ((and (null? actualParams) (eq? (firstLayer funcState) (firstLayer state))) (firstLayer (nextLayers funcState)))
      ((null? actualParams) (firstLayer funcState))
      ((eq? (firstLayer funcState) (firstLayer state)) (setActualParams actualParams formalParams state (nextLayers funcState) return continue break throw type))
      ((and (and (eq? (car actualParams) 'this) (inState? 'this state)) (eq? (unbox currentInstance) 'super)) (setActualParams (cdr actualParams) (cdr formalParams) state funcState return continue break throw type))
      ((and (eq? (car actualParams) 'this) (inState? 'this state))
       (if (inState? (unbox currentInstance) state) (setActualParams (cdr actualParams) (cdr formalParams) state (addToState 'this (searchState (unbox currentInstance) state) funcState) return continue break throw type)
           (setActualParams (cdr actualParams) (cdr formalParams) state (addToState 'this (searchState 'this state) funcState) return continue break throw type)))
      ((eq? (car actualParams) 'this) (setActualParams (cdr actualParams) (cdr formalParams) state (addToState 'this (searchState (unbox currentInstance) state) funcState) return continue break throw type))
      ((and (inState? (car formalParams) (nextLayers funcState)) (eq? (firstLayer funcState) (firstLayer state)))
       (setActualParams (cdr actualParams) (cdr formalParams)
                        state
                        (replaceInState (car formalParams) (mathValue (car actualParams) type funcState return continue break throw) state) return continue break throw type))
      ((and (list? (car actualParams)) (eq? (caar actualParams) 'dot) (setActualParams (cdr actualParams) (cdr formalParams) state (addToState (car formalParams) (dotGetFieldValue (car actualParams) funcState) funcState) return continue break throw type)))
      (else (setActualParams (cdr actualParams) (cdr formalParams) state (addToState (car formalParams) (mathValue (car actualParams) type state return continue break throw) funcState) return continue break throw type)))))

;setVariablesInScope takes the state when a function is called and a layer
;containing the initialized actual parameters of the function and returns
;a new layer containing all variables that should be in the scope of the function
(define setVariablesInScope
  (lambda (state paramLayer type)
    (cond
      ((null? (nextLayers state)) paramLayer)
      ((eq? (firstLayer (nextLayers state)) stateEmpty) paramLayer)
      ((inLayer? type (firstLayer (nextLayers state))) paramLayer)
      (else (setVariablesInScope (nextLayers state) (mergeLayers (firstLayer (nextLayers state)) paramLayer) type)))))

;mergeLayers merges two layers into one containing all values from both
;values from the stateLayer are overwritten by values in paramLayer
(define mergeLayers
  (lambda (stateLayer paramLayer)
    (cond
      ((null? (car stateLayer)) paramLayer)
      ((null? (car paramLayer)) stateLayer)
      ((inLayer? (caar stateLayer) paramLayer) (mergeLayers (removeFromLayer (caar stateLayer) stateLayer) paramLayer))
      (else (mergeLayers (removeFromLayer (caar stateLayer) stateLayer) (addToLayer (caar stateLayer) (car (cadr stateLayer)) paramLayer))))))

;evaluateStatement takes a statement and a state and returns the new state after
;evaluating the statement
(define evaluateStatement
  (lambda (statement state type return continue break throw)
    (cond
      ((null? statement) state)
      ((eq? (langValue statement) 'function) (bindFunctionClosure statement type state))
      ((and(and (and (eq? (langValue statement) 'funcall) (eq? (caadr statement) 'dot)) (list? (cadr (cadr statement)))) (eq? (caadr (cadr statement)) 'new))
       (set-box! currentInstance (cadr (cadr statement)))
       (evalDotFunction (cadr statement) (cons 'this (getActualParams statement)) state return continue break throw (car (createInstanceClosure (trueType (cadr (cadr statement))) state))))
      ((and (and (eq? (langValue statement) 'funcall) (eq? (caadr statement) 'dot)) (eq? (cadr (cadr statement)) 'super))
       (set-box! currentInstance (cadr (cadr statement)))
       (if (inLayer? 'this (firstLayer state)) (evalDotFunction (cadr statement) (cons 'this (getActualParams statement)) (replaceInState 'this type (addToState 'super (setSuperClassFields (caddr type) type state) state)) return continue break throw type)
           (evalDotFunction (cadr statement) (cons 'this (getActualParams statement)) (addToState 'super (setSuperClassFields (caddr type) type state) state) return continue break throw type)))
      ((and (eq? (langValue statement) 'funcall) (eq? (caadr statement) 'dot))
       (set-box! currentInstance (cadr (cadr statement)))
       (if (inState? 'super state) (evalDotFunction (cadr statement) (cons 'this (getActualParams statement)) state return continue break throw (caddr type))
       (evalDotFunction (cadr statement) (cons 'this (getActualParams statement)) state return continue break throw type)))
      ((eq? (langValue statement) 'funcall)
       (evalFunction (searchState (funcName statement) state) (getActualParams statement) state return continue break throw))
      ((eq? (langValue statement) 'begin) (stateBeginBlock (wholeBody statement) state return continue break throw type))
      ((eq? (langValue statement) 'break) (break (statePopLayer state)))
      ((eq? (langValue statement) 'continue) (stateContinue state continue))
      ((eq? (langValue statement) 'try)  (stateTry statement state return continue break throw))
      ((eq? (langValue statement) 'throw) (throw (mathValue (throwStatement statement) type state return continue break throw) state))
      ((eq? (langValue statement) 'finally) (runBlock (blockBody statement) state return continue break throw type)) ;executes code within regardless of rest of the try block, has try block scope
      ((eq? (langValue statement) 'return) (returnValue (returnExp statement) state return continue break throw type))
      ((eq? (langValue statement) 'while) (stateWhile (whileConditon statement) (whileBody statement) state return continue break throw))
      ((eq? (langValue statement) 'var) (stateDeclare (declareExp statement) state return continue break throw type))
      ((eq? (langValue statement) '=)
       (set-box! newState state)
       (stateAssign (assignExp statement) state return continue break throw type))
      ((eq? (langValue statement) 'if) (stateIf (ifCondition statement) (thenStatement statement) (elseStatement statement) state return continue break throw type))
      (else (error "Incorrect syntax")))))

;begins a block statement by creating a new layer on the state
;and running the body
(define stateBeginBlock
  (lambda (expression state return continue break throw type)
    (runBlock expression (cons stateEmpty state) return continue break throw type)))

;runs a block of code line by line and returns the state
(define runBlock
  (lambda (block state return continue break throw type)
    (if (null? block)
      state
      (runBlock (nextLines block) (evaluateStatement (currentLine block) state type return continue break throw) return continue break throw type))))

;stateTry runs a try-catch-finally block by first checking which parts of the block are
;present in the code, then running them
(define stateTry
  (lambda (statement state return continue break throw)
    (cond
      ((and (null? (catchBody statement)) (null? (finallyBlock statement))) (tryCatchFinally (blockBody statement) NULL NULL state return continue break throw)) ;no catch, no finally
      ((null? (catchBody statement)) (tryCatchFinally (blockBody statement) NULL (finallyBlock statement) state return continue break throw)) ;no catch, try, finally
      ((null? (finallyBlock statement)) (tryCatchFinally (blockBody statement) (catchBody statement) NULL state return continue break throw)) ;no finally, try, catch
      (else (tryCatchFinally (blockBody statement) (catchBody statement) (finallyBlock statement) state return continue break throw)))))

;tryCatchFinally runs each section of a try-catch-finally block creating continuations along
;the way
(define tryCatchFinally
  (lambda (tryBody catchBody finallyBody state return continue break throw)
    (cond
      ((null? finallyBody)
       (call/cc
        (lambda (newThrow)
          (run tryBody state return continue break (lambda (val newState) (newThrow (stateCatch catchBody val newState return continue break throw)))))))
      (else (run finallyBody
                       (call/cc
                        (lambda (newThrow)
                          (run tryBody state return continue break (lambda (val newState) (newThrow (stateCatch catchBody val newState return continue break throw)))))) return continue break throw)))))

 ;creates a new block and scope for catch body with the thrown variable
(define stateCatch
  (lambda (body val state return continue break throw type)
    (if (null? body)
      state
      (runBlock (catchBody body) (addToState (catchValue body) val (unbox newState)) return continue break throw type))))

;stateContinue calls the continue continuation and pops the layer of the loop
(define stateContinue
  (lambda (state continue)
    (continue (statePopLayer state))))

;stateIf takes a condition, an if statement, an else statement, and a state
;and returns the new state after evaluating either of the statements depending on
;the condition
(define stateIf
  (lambda (condition statement else state return continue break throw type)
    (if (mathValue condition type state return continue break throw)
        (evaluateStatement statement state type return continue break throw)
        (evaluateStatement else state type return continue break throw))))

;stateDeclare takes a statement containing a variable and possibly an assignment
;and returns the new state with the variable declared
(define stateDeclare
  (lambda (statement state return continue break throw type)
    (cond
      ((null? statement) state)
      ((null? (declareExp statement)) (addToState (variable statement) 'unassigned (removeFromState (variable statement) state)))
      (else (addToState (variable statement) (mathValue (assignmentExp statement) type state return continue break throw) (removeFromState (variable statement) state))))))

;stateAssign takes a statement containing a variable and an expression and returns
;the new state with the variable assigned the value of the expression
(define stateAssign
  (lambda (statement state return continue break throw type)
    (cond
      ((null? statement) state)
      ((list? (variable statement)) (replaceInState (cadr (variable statement)) (setFieldValue (searchState (cadr (variable statement)) state) (mathValue (assignmentExp statement) type state return continue break throw) (caddr (variable statement)))(unbox newState)))
      ((inLayer? (variable statement) (cadr type)) (addToState 'this (setFieldValue type (mathValue (assignmentExp statement) type state return continue break throw) (variable statement)) state))
      (else (replaceInState (variable statement) (mathValue (assignmentExp statement) type state return continue break throw) state)))))

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

;stateWhile loop executes the code body iteration by iteration creating
;continue continuations until the loop condition is false
(define stateWhileLoop
  (lambda (condition body state return continue break throw type)
    (if (mathValue condition type state return continue break throw)
        (stateWhileLoop condition body (call/cc
                                        (lambda (newContinue)
                                          (evaluateStatement body state return newContinue break throw)))
                        return continue break throw)
        state)))

;returnValue takes an expression and a state and returns the value of the expression
;as an integer or a boolean
(define returnValue
  (lambda (expression state return continue break throw type)
    (set-box! newState state)
    (let ((val (mathValue expression type state return continue break throw)))
      (cond
        ((number? val) (return val))
        ((boolean? val) (return (boolValue val)))
        (else (return val))))))

;mathValue takes an expression and returns it's mathematical value (integer or boolean)
(define mathValue
  (lambda (exp type state return continue break throw)
    (cond
      ;null/error checks
      ((null? exp) exp)
      ((number? exp) exp) ;no futher recursion needed, return number value
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((not (list? exp)) (searchForFieldValue exp type state)) ;not  number, yet not a list...must be a variable!
      ((number? (operator exp)) (error "Invalid expression")) ;the expression has no operator :(
      ((eq? '!= (operator exp)) (not (= (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw))))
      ((eq? '! (operator exp)) (not (mathValue (operand1 exp) type state return continue break throw)))
      ((and (eq? (operator exp) '-) (null? (binaryExp exp))) (- 0 (mathValue (operand1 exp) type state)))
      ((eq? (operator exp) 'new) (createInstanceClosure (trueType exp) state))
      ((and (eq? (operator exp) 'funcall) (list? (cadr exp)) (eq? (caadr exp) 'dot) (list? (cadr (cadr exp))) (eq? (caadr (cadr exp)) 'new))
       (set-box! currentInstance (trueType (cadr (cadr exp))))
       (returnDotFunctionValue (cadr exp) (cons 'this (getActualParams exp)) state return continue break throw (car (createInstanceClosure (trueType (cadr (cadr exp))) state))))
      ((and (and (eq? (operator exp) 'funcall) (list? (cadr exp)) (eq? (caadr exp) 'dot)) (eq? (cadr (cadr exp)) 'super))
       (set-box! currentInstance (cadr (cadr exp)))
       (if (inLayer? 'this (firstLayer state)) (returnDotFunctionValue (cadr exp) (cons 'this (getActualParams exp)) (replaceInState 'this type (addToState 'super (setSuperClassFields (caddr type) type state) state)) return continue break throw type)
           (returnDotFunctionValue (cadr exp) (cons 'this (getActualParams exp)) (addToState 'super (setSuperClassFields (caddr type) type state) state) return continue break throw type)))
      ((and (eq? (operator exp) 'funcall) (list? (cadr exp)) (eq? (caadr exp) 'dot))
       (set-box! currentInstance (cadr (cadr exp)))
       (if (inLayer? 'this (firstLayer state)) (returnDotFunctionValue (cadr exp) (cons 'this (getActualParams exp)) state return continue break throw (searchState 'this state))
       (returnDotFunctionValue (cadr exp) (cons 'this (getActualParams exp)) state return continue break throw type)))
      ((eq? (operator exp) 'funcall)
       (returnFunctionValue (searchState (cadr exp) type) (cons 'this (getActualParams exp)) state return continue break throw type))
      ((eq? (operator exp) 'dot)
       (set-box! currentInstance (cadr exp))
       (dotGetFieldValue exp state))
      ((or (eq? (mathValue (operand1 exp) type state return continue break throw) 'unassigned) (eq? (mathValue (operand2 exp) type state return continue break throw) 'unassigned)) (error "Variable has not been assigned a value"))
      ;&&/||/! evaluation, needs to be in format (operator bool bool) else bad logic
      ((eq? '&& (operator exp)) (and (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '|| (operator exp)) (or (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ;boolean evaluation
      ((eq? '== (operator exp)) (equal? (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '<= (operator exp)) (<= (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '>= (operator exp)) (>= (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '<  (operator exp))  (< (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '>  (operator exp))  (> (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ;math evaluation
      ((eq? '+ (operator exp)) (+ (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '- (operator exp)) (- (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '* (operator exp)) (* (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '/ (operator exp)) (quotient (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      ((eq? '% (operator exp)) (remainder (mathValue (operand1 exp) type state return continue break throw) (mathValue (operand2 exp) type state return continue break throw)))
      (else (error "Unknown Operator")))))

(define trueType cadr)

(define setSuperClassFields
  (lambda (super instance state)
    (cons (car super) (mergeLayers (cadr super) (cadr instance)))))

(define searchForFieldValue
  (lambda (var instance state)
    (cond
      ((and (inState? 'this state) (checkMain))
       (if (null? (searchInstance var state))
           (if (null? (searchInstance var (list (cadr (searchState 'this state)))))
               (searchState var (list (cadr instance)))
               (searchInstance var (list (cadr (searchState 'this state)))))
           (searchInstance var state)))
       (else
        (if (null? (searchInstance var state))
           (searchState var (list (cadr instance)))
           (searchInstance var state))))))
(define searchInstance
  (lambda (var state)
    (cond
      ((null? state) state)
      ((null? (firstLayer state)) '())
      ((or (null? (nameBindings state)) (null? (valueBindings state))) (searchInstance var (nextLayers state)))
      ((eq? var (searchCurrentName state)) (searchCurrentValue state))
      ((null? (restOfNames (nameBindings state))) (searchInstance var (nextLayers state)))
      (else (searchInstance var (cons (concatNamesAndValues (searchNext (nameBindings state)) (searchNext (valueBindings state))) (nextLayers state)))))))




;addToState takes a variable and data and adds it to a state
(define addToState
  (lambda (var data state)
    (if (null? state)
      (cons (concatNamesAndValues (list var) (list data)) (nextLayers state))
      (cons (concatNamesAndValues (addVarName var state) (addVarValue data state)) (nextLayers state)))))

(define addToLayer
  (lambda (var data layer)
    (list (cons var (car layer)) (cons data (cadr layer)))))

;addToState helpers
(define addVarName
  (lambda (var state)
    (cons var (nameBindings state))))

(define addVarValue
  (lambda (data state)
    (cons data (valueBindings state))))

(define concatNamesAndValues list)

;searchState takes a name and a state and returns associated data or procedure
(define searchState
  (lambda (var state)
    (cond
      ((null? state) (error "Variable/Function not in scope"))
      ((not (list? (car (car state)))) (searchState var (list state)))
      ((or (null? (nameBindings state)) (null? (valueBindings state))) (searchState var (nextLayers state)))
      ((eq? var (searchCurrentName state)) (searchCurrentValue state))
      ((null? (restOfNames (nameBindings state))) (searchState var (nextLayers state)))
      (else (searchState var (cons (concatNamesAndValues (searchNext (nameBindings state)) (searchNext (valueBindings state))) (nextLayers state)))))))

;searchState abstraction
(define restOfNames cdr)

(define inState?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((inLayer? var (firstLayer state)) #t)
      (else (inState? var (nextLayers state))))))

(define inLayer?
  (lambda (var layer)
    (cond
      ((null? layer) #f)
      ((null? (car layer)) #f)
      ((eq? (caar layer) var) #t)
      (else (inLayer? var (list (cdr (car layer)) (cdr (cdr layer))))))))

;replaceInstate takes a variable, a value, and a state and returns the new state
;with that variable's value replaced with the given value
(define replaceInState
  (lambda (var val state)
    ;recurse through layer, if it's empty, go to the next layer
    ;if variable found, replace it, otherwise keep going through layer
    (cond
      ((null? state) (error "Variable not in scope"))
      ((inLayer? var (firstLayer state)) (cons (replaceInLayer var val (firstLayer state)) (nextLayers state)))
      (else (cons (firstLayer state) (replaceInState var val (nextLayers state)))))))

; replaceInLayer
; Given a variable name, value, and layer, find the location within the layer
; where the given variable name is stored and replace its value, and return the new layer
(define replaceInLayer
  (lambda (var val layer)
    (replaceInLayer-cps var val (car layer) (cadr layer) (lambda (l1 l2) (list l1 l2)))))

; tail recursive helper for replaceInLayer
(define replaceInLayer-cps
  (lambda (var val names values return)
    (cond
      ((null? names) (return names values))
      ((equal? var (car names)) (return names (cons val (cdr values))))
      (else (replaceInLayer-cps var val (cdr names) (cdr values) (lambda (l1 l2) (return (cons (car names) l1) (cons (car values) l2))))))))

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


(define currentInstance (box '()))
(define firstBlock car)
(define secondBlock cadr)
(define firstVar car)
(define remainingVars cdr)
(define functionName cadr)
(define functionParams caddr)
(define functionBody cadddr)
(define getFormalParams car)
(define getStateFunc caddr)
(define getFunctionBody cadr)
(define nextLines cdr)
(define currentLine car)
(define langValue car)
(define declareExp cdr)
(define returnExp cadr)
(define assignExp cdr)
(define blockBody cadr)
(define throwStatement cadr)
(define funcName cadr)
(define catchName (lambda (statement) (car (cadr statement))))
(define wholeBody cdr)
(define statePopLayer
  (lambda (state)
    (cdr state)))
(define stateNewLayer
  (lambda (state)
    (cons stateEmpty state)))
(define finallyBlock cdddr)
(define catchBody caddr)
(define NULL (list))
(define catchValue caadr)
(define ifCondition cadr)
(define thenStatement caddr)
(define elseStatement
  (lambda (statement)
    (if (null? (cdddr statement))
        '()
        (cadddr statement))))
(define whileConditon cadr)
(define whileBody caddr)
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define getActualParams
  (lambda (exp)
    (cddr exp)))
(define boolValue
  (lambda (bool)
    (if bool 'true 'false)))
(define binaryExp cddr)
;initial state, no variables declared or assigned, abstracted!
(define stateEmpty
  (list (list) (list)))
(define newState (box stateEmpty))
;bindings to the names of variables in the state
(define nameBindings
  (lambda (state)
    (car (firstLayer state))))
;bindings to the values of variables in the state
(define valueBindings
    (lambda (state)
      (cadr (firstLayer state))))
(define firstLayer car)
(define nextLayers cdr)
(define searchNext cdr)
(define mainClass (box '()))
(define searchCurrentName
  (lambda (state)
    (caar (firstLayer state))))

(define searchCurrentValue
  (lambda (state)
    (caadr (firstLayer state))))
(define checkMain
  (lambda ()
    (if (<= (string-length (symbol->string(unbox mainClass))) 1)
        #f)))
(define nextVar
  (lambda (layer)
    (list (remainingVars (firstBlock layer)) (remainingVars (secondBlock layer)))))
(define listLength
  (lambda (l n)
    (cond
      ((null? l) n)
      (length (cdr l) (+ 1 n)))))
(define defaultContinue
  (lambda (state)
    (error "Can't call continue here")))
(define defaultBreak
  (lambda (state)
    (error "Can't call break here")))
(define defaultThrow
  (lambda (val state)
    (error "Thrown statement not caught")))


(define verbose #f)
(define logln
  (lambda (title val)
    (if verbose
        (begin (display title)(display ":")(display val)(newline)(newline)))))
