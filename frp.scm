(import scheme chicken)
(use gochan srfi-18 srfi-1)

;; Helpers

(define (gochan-broadcast chans msg)
  (for-each (cut gochan-send <> msg) chans))


;; Low-level implementation

(include "dispatcher")
(include "primitive-signal")


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
