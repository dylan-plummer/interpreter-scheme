#lang racket

(require rackunit
         "interpreter.rkt")

(check-equal? (interpret "test_program") 2000000 "Test 20")
