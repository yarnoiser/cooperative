; -----------------------------------------------------------------------------
; cooperative.import.scm
;
; Coroutines and Finite State Machines
; -----------------------------------------------------------------------------
(module cooperative (make-coroutine in-coroutine? yield! fsm)
  (import chicken scheme)
  (use matchable)
  (import-for-syntax matchable)

  ; continuation stack
  (define conts '())

  ; value returned by a yield
  (define ret-val (void))

  ; flag used to determine if a continuation returned due to a yield
  (define yield-occured #f)

  (define (cont-push! c)
    (set! conts (cons c conts)))

  (define (cont-pop!)
    (let ([c (car conts)])
      (set! conts (cdr conts))
      c))

  (define (in-coroutine?)
    (not (null? conts)))

  (define (make-coroutine proc . args)
    (let ([cont (lambda (return)
                  (cont-push! return)
                  (let ([res (apply proc args)])
                    ((cont-pop!) res)))]
          [finished #f])
      (lambda ()
        (if finished
          (error "coroutine has finished"))
        (set! cont (call/cc cont))
        (if yield-occured
          (begin (set! yield-occured #f)
                 ret-val)
          (begin (set! finished #t)
                 cont)))))

  (define (yield! #!optional [val (void)])
    (if (not (in-coroutine?))
      (error "not within coroutine"))
    (set! ret-val val)
    (set! yield-occured #t)
    (cont-push! (call/cc (cont-pop!))))

  (define-syntax fsm (ir-macro-transformer 
    (lambda (expression inject compare?) 
      (match expression 
        [(_ args vars initial-state . body) 
         `(let ,vars 
            (let* ([state ,initial-state] 
                   [,(inject 'trans!) (lambda (new-state) (set! state new-state))]) 
                (values (lambda ,args 
                          (case state . 
                          ,(map (lambda (current-body) 
                                `((,(car current-body)) . ,(cdr current-body))) 
                           body)))
                       ,(inject 'trans!))))]))))
)

