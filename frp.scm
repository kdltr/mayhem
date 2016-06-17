(import scheme chicken)
(use gochan srfi-18 srfi-1)

;; Helpers

(define (gochan-broadcast chans msg)
  (for-each (cut gochan-send <> msg) chans))


;; Low-level implementation

(define-record-type primitive-signal
  (%make-primitive-signal receptor emitters default-value identifier thread)
  primitive-signal?
  (receptor primitive-receptor primitive-receptor-set!)
  (emitters primitive-emitters primitive-emitters-set!)
  (default-value default-value)
  (identifier identifier identifier-set!)
  (thread primitive-thread primitive-thread-set!))


(define inputs-pool
  '())

(define dispatcher-channel
  (gochan))

(define dispatcher
  (make-thread
   (lambda ()
     (let loop ()
       (let ((msg (gochan-receive dispatcher-channel)))
         (print (list 'broadcasting inputs-pool))
         (gochan-broadcast inputs-pool msg))
       (loop)))))

(thread-start! dispatcher)

(define (register-primitive-signal! signal)
  (let ((channel (gochan)))
    (set! inputs-pool (cons channel inputs-pool))
    (primitive-receptor-set! signal channel)))

(define (primitive-signal-loop signal)
  (lambda ()
    (let loop ((state (default-value signal)))
      (print "looping")
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


;; (define (make-signal-thread! sig primitive?)
;;   (signal-thread-set!
;;    sig
;;    (make-thread
;;     (if primitive?
;;         (lambda ()
;;           (let loop ((state #f))
;;             (let* ((messages (map gochan-receive (receptors sig)))
;;                    (out-msg (apply (signal-function sig) state messages)))
;;               (for-each (lambda (c) (gochan-send c out-msg)) (emitters sig))
;;               (loop (cadr out-msg)))))
;;         (lambda ()
;;           (let loop ((state #f))
;;             (let* ((inputs (map gochan-receive (receptors sig)))
;;                    (change? (any change-message? inputs))
;;                    (messages (and change? (map cadr inputs)))
;;                    (new-state (if change? (apply (signal-function sig) state messages) state))
;;                    (out-msg (list (if change? 'change 'no-change) new-state)))
;;               (for-each (lambda (c) (gochan-send c out-msg)) (emitters sig))
;;               (loop new-state))))))))

;; (define (change-message? o)
;;   (and (pair? o) (eq? 'change (car o))))

;; (define (start-signal-graph! signal)
;;   (let ((thread (signal-thread signal)))
;;     (case (thread-state thread)
;;       ((created) (thread-start! thread))
;;       ((ready running blocked sleeping) (void)) ;; thread already started
;;       (else  (error "thread in strange state" signal thread))))
;;   (for-each start-signal-graph! (parents signal)))

;; ;; TODO do that better, this is ugly
;; (define (terminate-signal-graph! signal)
;;   (let ((thread (signal-thread signal)))
;;     (thread-terminate! thread)
;;     (for-each terminate-signal-graph! (parents signal))))

;; (define (make-primitive-signal function)
;;   (let* ((input-channel (gochan))
;;          (sig (%make-signal (list input-channel) '() '() function #f)))
;;     (register-primitive-channel! input-channel)
;;     (make-signal-thread! sig #t)
;;     sig))


;; (define-record-type signal
;;   (%make-signal receptors emitters parents function thread)
;;   signal?
;;   (receptors receptors receptors-set!)
;;   (emitters emitters emitters-set!)
;;   (parents parents)
;;   (function signal-function)
;;   (thread signal-thread signal-thread-set!))


;; (define (make-registered-signal parents function)
;;   (let* ((channels (map (lambda (_) (gochan)) parents))
;;          (sig (%make-signal channels '() parents function #f)))
;;     (for-each
;;      (lambda (s c)
;;        (emitters-set! s (cons c (emitters s))))
;;      parents
;;      channels)
;;     (make-signal-thread! sig #f)
;;     sig))
