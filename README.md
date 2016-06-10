# cooperative
This README is a work in progress.

Coroutines and Finite State Machines for Chicken Scheme


## Installing
Run *chicken-install cooperative*
(will work once egg is submitted).

## Usage
### \[procedure] (make-coroutine proc . args)
Returns a new coroutine. The resulting procedure is a thunk
(it takes no arguments). When the coroutine is called, it is equivalent to applying
*args* to *proc*, except that any calls to *yield!*
will return controll to the calling procedure. When the coroutine is called again
after *yield!*, it will continue where it left off before yielding
An error is signalled if the coroutine is called after it completes
without calling *yield!*.

#### Example
```scheme
(define from-1-to-3 (make-coroutine (lambda (x y)
                                      (let loop ()
                                        (if (< x y)
                                          (begin (set! x (add1 x))
                                                 (yield! x) 
                                                 (loop))))) 0 3))

(from-1-to-3)
1
(from-1-to-3)
2
(from-1-to-3)
3
(from-1-to-3)
error "coroutine has finished"
```

### \[procedure] (in-coroutine?)
Returns true if it is called within a coroutine created with a call
to *make-coroutine*. Else returns false.

#### Example

### \[procedure] (yield! [val])
Exits the current coroutine returning control to the calling
procedure. Returns the optional value *val*, which defaults to (void). An error
is signaled if *yield!* is called outside of a coroutine procedure.

#### Example

### \[syntax] (fsm input: (input ...) vars: vars start: initial-state state ...)
Creates a finite state machine. The resulting machine is a procedure which
behaves differently with subsequent calls depending on what state it is in.

The machine parameters consist of literals followed by a relevant expression.
these parameters must be outlined in the order they are shown above.

**input:** Followed by a list of input values. These are received as arguments
to the finite state machine when it is called, and can be referenced as
variables within the state machine.

**vars:** Followed by a list of variable assignments equivalent to those found
in a *let* expression. These variables are internal to the state machine and
persist between calls to it, unless specifically changed.

**start:** Followed by the name of the initial state. This is the state in
which the finite state machine will be in when it is first called. The name of
the initial state is not evaluated, and does not need to be quoted.

The remaining parameters are state definitions, which also consist of literals
followed by relevant expressions. Like the above parameters, they must be
written in the order shown:

**(state: state-name act: act output: (output ...) trans: ((pred next-state) ...))**

**state:** Followed by the state name. This is the name of the state being
defined and is used to refer to this state during transitions. This name is not
evaluated, and does not need to be quoted.

**act:** Followed by the action to be performed for this state. This consists
of a single scheme expression. Forms such as *let* or *begin* should be used
to perform multiple actions here.

**output:** Followed by a list of output values. A list containing all values
specified here will be returned to the calling procedure. The list should not
be quoted, quasiquoted or created with the list procedure, simply specified
with parentheses, although individual elements will be evaluated.

**trans:** Followed by the transition list. This is an association list
containing predicates paired with states to transition to. The next time the
finite state machine is called, the it will be in the state paired with the
first predicate to return true. The state names are not evaluated and do not
need to be quoted.

#### Example
```scheme
(define docking-control
  (fsm
    input: (use-case)
    vars: ((ports 3) (ships 0) (status #f))
    start: empty

    (state: empty
      act: (case use-case
             ((arrival) (begin (set! ships (add1 ships))
                               (set! status 'approved)))
             ((departure) (set! status 'denied))
             (else (error "invalid use case")))
      output: (status)
      trans: (((< 0 ships) else)))

    (state: full
      act: (case use-case
             ((arrival) (set! status 'denied))
             ((departure) (begin (set! ships (sub1 ships))
                                 (set! status 'approved)))
             (else (error "invalid use case")))
      output: (status)
      trans: (((< ships ports) else)))

    (state: else
      act: (begin (set! status 'approved)
                  (case use-case
                    ((arrival) (set! ships (add1 ships)))
                    ((departure) (set! ships (sub1 ships)))
                    (else (error "invalid use case"))))
      output: (status)
      trans: (((= ports ships) full)
              ((= ships 0) empty))) ))
```

(docking-control 'departure)
(denied)

(docking-control 'arrival)
(approved)

