(define-record-type signal
  (%make-signal receptors emitters parents function thread initial-value)
  signal?
  (receptors receptors receptors-set!)
  (emitters emitters emitters-set!)
  (parents parents)
  (function signal-function)
  (thread signal-thread signal-thread-set!)
  (initial-value %initial-value))

(define (initial-value o)
  (and (signal? o) (%initial-value o)))

(define (signal-loop sig)
  (lambda ()
    (let loop ((state (initial-value sig)))
      (let* ((inputs (map gochan-receive (receptors sig)))
             (change? (any change-message? inputs))
             (messages (and change? (map cadr inputs)))
             (new-state (if change? (apply (signal-function sig) state messages) state))
             (out-msg (list (if change? 'change 'no-change) new-state)))
        (gochan-broadcast (emitters sig) out-msg)
        (loop new-state)))))

(define (make-registered-signal parents function)
  (let* ((parents-defaults (map (lambda (s) (or (default-value s) (initial-value s))) parents))
         (init (apply function #f parents-defaults))
         (channels (map (lambda (_) (gochan)) parents))
         (sig (%make-signal channels '() parents function #f init))
         (signal-thunk (signal-loop sig)))
    (for-each register-emitter! parents channels)
    (signal-thread-set! sig (make-thread signal-thunk))
    sig))
