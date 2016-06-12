(import cooperative)
(use simple-exceptions)

; create coroutine
(define proc* (make-coroutine (lambda ()
                                (yield!)
                                2)))
(define proc (make-coroutine (lambda (x y)
                               (yield! (+ x y))
                               (yield! (/ x y))
                               (yield! (proc*))
                               (yield! (proc*))
                               (* x y)) 2 3))

; procedure yields properly
(assert (= (proc) (+ 2 3)))

; procedure yields properly more than once
(assert (= (proc) (/ 2 3)))

; proper yield from nested coroutine
(assert (eqv? (proc) (void)))

; normal return value from nested procedure
(assert (= (proc) 2))

; we receive normal return value from procedure
(assert (= (proc) (* 2 3)))

; check error is received when coroutine is finished
(let ([err #f])
  (with-exn-handler (lambda (e)
                      (set! err #t)
                      (if (not (equal? (message e) "coroutine has finished"))
                        (error "improper error for finished coroutine procedure")))
                    (lambda ()
                      (proc)))
  (if (not err)
    (error "no error from finished coroutine procedure")))

; test in-coroutine?

; should be false
(define (test-proc)
  (in-coroutine?))

; should be true
(define test-coroutine (make-coroutine test-proc))

(assert (not (test-proc)))
(assert (test-coroutine))

; create finite state machine
(define f (fsm
            input: (input1 input2)
            vars: ([count1 0] [count2 0])
            start: state1

            (state: state1
              act: (void)
              output: (input1 input2)
              trans: ((#t state2)))

            (state: state2
              act: (set! count1 (add1 count1))
              output: (count1 count2)
              trans: (((= count1 2) state3)))

            (state: state3
              act: (set! count2 input2)
              output: (count1 count2)
              trans: (((= count2 500) state4)))

            (state: state4
              act: (void)
              output: (count2)
              trans: ())))

; input and output work
(assert (equal? (f 0 1) '(0 1)))

; transitioned to state 2 and variable is properly incremented
(assert (equal? (f 0 1) '(1 0)))

; properly stayed in state 2 and continued incrementing
(assert (equal? (f 0 1) '(2 0)))

; properly transitioned to state 3 and properly set count2
(assert (equal? (f 0 1) '(2 1)))

; properly stayed in state where no transitions take place and set count2 again
(assert (equal? (f 0 500) '(2 500)))

; properly transition and count2 is still what it was set to
(assert (equal? (f 0 0) '(500)))


