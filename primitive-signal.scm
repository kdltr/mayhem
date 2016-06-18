(define-record-type primitive-signal
  (%make-primitive-signal receptor emitters default-value identifier thread)
  primitive-signal?
  (receptor primitive-receptor primitive-receptor-set!)
  (emitters primitive-emitters primitive-emitters-set!)
  (default-value default-value)
  (identifier identifier identifier-set!)
  (thread primitive-thread primitive-thread-set!))



(define (primitive-signal-loop signal)
  (lambda ()
    (let loop ((state (default-value signal)))
      (let* ((message (gochan-receive (primitive-receptor signal)))
             (is-target? (eq? (car message) (identifier signal)))
             (new-state (if is-target? (cadr message) state))
             (out-msg (list (if is-target? 'change 'no-change) new-state)))
        (gochan-broadcast (primitive-emitters signal) out-msg)
        (loop new-state)))))

(define (make-primitive-signal default-value)
  (let* ((identifier (gensym 'primitive-signal))
         (signal (%make-primitive-signal #f '() default-value identifier #f))
         (thread-thunk (primitive-signal-loop signal)))
    (register-primitive-signal! signal)
    (primitive-thread-set! signal (make-thread thread-thunk))
    signal))

(define (notify-primitive-signal! signal msg)
  (gochan-send dispatcher-channel (list (identifier signal) msg)))
