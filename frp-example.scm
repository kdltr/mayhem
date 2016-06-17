(use glfw3 gl nonblocking-swap-buffers matchable)

(load "frp")

;; (use trace)
;; (trace gochan-send gochan-receive)

(define new-frame
  (make-primitive-signal
   (lambda (state message)
     (if (equal? message 'new-frame)
         '(change new-frame)
         '(no-change new-frame)))))

(define cursor-position
  (make-primitive-signal
   (lambda (state message)
     (match message
       (('cursor-position x y)
        (list 'change (list x y)))
       (else
        (list 'no-change state))))))

;; (define clock (signal-map get-time new-frame))
;; or something like that
(define clock
  (make-registered-signal
   (list new-frame)
   (lambda (state new-frame)
     (get-time))))

(define scene
  (make-registered-signal
   (list clock cursor-position)
   (lambda (state clock cursor-position)
     ;; represent scene as a thunk for now
     (lambda ()
       (print cursor-position)
       (gl:Clear gl:COLOR_BUFFER_BIT)
       (gl:LoadIdentity)
       (gl:Rotatef (* 100 clock) 0 0 1)
       (gl:Begin gl:QUADS)
       (gl:Vertex2f -0.2 0.2)
       (gl:Vertex2f 0.2 0.2)
       (gl:Vertex2f 0.2 -0.2)
       (gl:Vertex2f -0.2 -0.2)
       (gl:End)))))

(define scene-receiver (gochan))
(emitters-set! scene (list scene-receiver))


(cursor-position-callback
 (lambda (window x y)
   (broadcast-message! (list 'cursor-position x y))))

(with-window (600 400 "GLFW3 Test" resizable: #f)
  (start-signal-graph! scene)
  (let loop ()
    (nonblocking-swap-buffers)
    (gc #f)
    (wait-vblank)
    ;; (swap-buffers (window))
    (poll-events)
    (broadcast-message! 'new-frame)
    (thread-yield!)
    (let loop2 ((msg (gochan-receive scene-receiver)))
      (if (eq? (car msg) 'change)
          (cadr msg)
          (loop2 (gochan-receive scene-receiver))))
    (unless (window-should-close (window))
      (loop))))
