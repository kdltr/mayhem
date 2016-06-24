(define-record-type primitive-signal
  (%make-primitive-signal receptor emitters initial-value identifier thread)
  primitive-signal?
  (receptor primitive-receptor primitive-receptor-set!)
  (emitters primitive-emitters primitive-emitters-set!)
  (initial-value primitive-initial-value)
  (identifier identifier identifier-set!)
  (thread primitive-thread primitive-thread-set!))

(define (primitive-signal-loop signal)
  (lambda ()
    (let loop ((state (primitive-initial-value signal)))
      (let* ((message (mailbox-receive! (primitive-receptor signal)))
             (is-target? (eq? (car message) (identifier signal)))
             (new-state (if is-target? (cadr message) state))
             (out-msg (list (if is-target? 'change 'no-change) new-state)))
        (broadcast! (primitive-emitters signal) out-msg)
        (loop new-state)))))

(define (make-primitive-signal initial-value)
  (let* ((identifier (gensym 'primitive-signal))
         (signal (%make-primitive-signal #f '() initial-value identifier #f))
         (thread-thunk (primitive-signal-loop signal)))
    (register-primitive-signal! signal)
    (primitive-thread-set! signal (make-thread thread-thunk))
    signal))

(define (notify-primitive-signal! signal msg)
  (mailbox-send! dispatcher-channel (list (identifier signal) msg)))
