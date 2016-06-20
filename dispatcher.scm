(define inputs-pool
  '())

(define dispatcher-channel
  (make-mailbox))

(define dispatcher
  (make-thread
   (lambda ()
     (let loop ()
       (let ((msg (mailbox-receive! dispatcher-channel)))
         (broadcast! inputs-pool msg))
       (loop)))))

(thread-start! dispatcher)

(define (register-primitive-signal! signal)
  (let ((channel (make-mailbox)))
    (set! inputs-pool (cons channel inputs-pool))
    (primitive-receptor-set! signal channel)))
