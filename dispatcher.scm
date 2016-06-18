(define inputs-pool
  '())

(define dispatcher-channel
  (gochan))

(define dispatcher
  (make-thread
   (lambda ()
     (let loop ()
       (let ((msg (gochan-receive dispatcher-channel)))
         (gochan-broadcast inputs-pool msg))
       (loop)))))

(thread-start! dispatcher)

(define (register-primitive-signal! signal)
  (let ((channel (gochan)))
    (set! inputs-pool (cons channel inputs-pool))
    (primitive-receptor-set! signal channel)))
