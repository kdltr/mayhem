(import scheme chicken)
(use gochan srfi-18 srfi-1)

(define-record-type signal
  (%make-signal receptors emitters parents function thread)
  signal?
  (receptors receptors receptors-set!)
  (emitters emitters emitters-set!)
  (parents parents)
  (function signal-function)
  (thread signal-thread signal-thread-set!))

(define primitive-channels
  (make-parameter '()))

(define (register-primitive-channel! chan)
  (primitive-channels (cons chan (primitive-channels))))

(define (broadcast-message! msg)
  (for-each (cut gochan-send <> msg) (primitive-channels)))

(define (make-signal-thread! sig function)
  (signal-thread-set!
   sig
   (make-thread
    (lambda ()
      (let loop ()
        (apply (signal-function sig) (cons* sig (emitters sig) (receptors sig)))
        (loop))))))

(define (start-signal-graph! signal)
  (let ((thread (signal-thread signal)))
    (case (thread-state thread)
      ((created) (thread-start! thread))
      ((ready running blocked sleeping) (void)) ;; thread already started
      (else  (error "thread in strange state" signal thread))))
  (for-each start-signal-graph! (parents signal)))

;; TODO do that better, this is ugly
(define (terminate-signal-graph! signal)
  (let ((thread (signal-thread signal)))
    (thread-terminate! thread)
    (for-each terminate-signal-graph! (parents signal))))

(define (make-primitive-signal function)
  (let* ((input-channel (gochan))
         (sig (%make-signal (list input-channel) '() '() function #f)))
    (register-primitive-channel! input-channel)
    (make-signal-thread! sig function)
    sig))

(define (make-registered-signal parents function)
  (let* ((channels (map (lambda (_) (gochan)) parents))
         (sig (%make-signal channels '() parents function #f)))
    (for-each
     (lambda (s c)
       (emitters-set! s (cons c (emitters s))))
     parents
     channels)
    (make-signal-thread! sig function)
    sig))
