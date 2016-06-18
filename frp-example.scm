(use glfw3 gl nonblocking-swap-buffers matchable)

(load "frp")

;; (use trace)
;; (trace gochan-send gochan-receive)

(define new-frame
  (make-primitive-signal 'new-frame))

(define cursor-position
  (make-primitive-signal '(0 0)))

;; (define clock (signal-map get-time new-frame))
;; or something like that
(define frame-time
  (make-registered-signal
   (list new-frame)
   (lambda (state new-frame)
     (get-time))))

(define scene
  (make-registered-signal
   (list frame-time cursor-position)
   (lambda (state clock cursor-position)
     ;; represent scene as a thunk for now
     (lambda ()
       (gl:Clear gl:COLOR_BUFFER_BIT)
       (gl:LoadIdentity)
       (let ((x (car cursor-position))
             (y (cadr cursor-position
                      )))
         (gl:Translatef (sub1 (/ x 300)) (- (sub1 (/ y 200))) 0))
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
   (notify-primitive-signal! cursor-position (list x y))))

(with-window (600 400 "GLFW3 Test" resizable: #f)
  (start-signal-graph! scene)
  (let loop ()
    (nonblocking-swap-buffers)
    (gc #f)
    (wait-vblank)
    ;; (swap-buffers (window))
    (poll-events)
    (notify-primitive-signal! new-frame 'new-frame)
    (thread-yield!)
    (let loop2 ((msg (gochan-receive* scene-receiver 0))
                (last void))
      (if (eq? msg #t)
          (last)
          (loop2 (gochan-receive* scene-receiver 0)
                 (cadar msg))))
    (unless (window-should-close (window))
      (loop))))
