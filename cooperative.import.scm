; Copyright (c) 2016, Robert Smiley
; All rights reserved.

; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice,
;    this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its contributors
;    may be used to endorse or promote products derived from this software
;    without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

; -----------------------------------------------------------------------------
; cooperative.import.scm
;
; Coroutines and Finite State Machines
; -----------------------------------------------------------------------------

(module cooperative (make-coroutine in-coroutine? yield! fsm)
  (import chicken scheme)

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

  (define-syntax fsm (syntax-rules (input: vars: start: state: act: trans: output:)
    ((fsm
       input: (input ...)
       vars: vars
       start: initial-state
       (state: state-name
         act: act
         output: (output ...)
         trans: ((pred next-state) ...))
        ...)

       (let vars
         (let ((state 'initial-state))
           (lambda args
             (receive (input ...) (apply values args)
               (case state
                 ((state-name) (begin act
                                     (cond
                                       (pred (set! state 'next-state))
                                        ...)
                                     (list output ...)))
                 ...))))))))
)

