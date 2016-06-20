(module frp-lowlevel
    *

  (import scheme chicken)
  (use mailbox srfi-18 srfi-1)

  ;; Helpers

  (define (broadcast! chans msg)
    (for-each (cut mailbox-send! <> msg) chans))

  (define (change-message? o)
    (and (pair? o) (eq? 'change (car o))))

  (define (register-emitter! signal channel)
    (let ((getter (cond ((primitive-signal? signal) primitive-emitters)
                        ((signal? signal) emitters)))
          (setter! (cond ((primitive-signal? signal) primitive-emitters-set!)
                         ((signal? signal) emitters-set!))))
      (setter! signal (cons channel (getter signal)))))


  ;; Low-level implementation

  (include "dispatcher")
  (include "primitive-signal")
  (include "signal")

  (define (start-signal-graph! signal)
    (let* ((accessor (or (and (primitive-signal? signal) primitive-thread)
                         (and (signal? signal) signal-thread)))
           (thread (accessor signal)))
      (case (thread-state thread)
        ((created) (thread-start! thread))
        ((ready running blocked sleeping suspended) (void)) ;; thread already started
        (else  (error "thread in strange state" signal thread (thread-state thread)))))
    (for-each start-signal-graph! (if (signal? signal) (parents signal) '()))))
