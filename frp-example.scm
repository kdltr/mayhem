(import scheme chicken data-structures srfi-18)
(use glfw3 gl nonblocking-swap-buffers matchable frp-lowlevel gochan frp)

(load "frp-glfw")
(import frp-glfw)

(define new-frame (make-primitive-signal 'new-frame))

(define frame-time (map (lambda (_) (get-time)) new-frame))
(define num-frames (fold + 0 (map (constantly 1) new-frame)))

(define square-angle (map (lambda (time) (* 100 time)) frame-time))
(define square-translation
  (map (lambda (cursor) (let ((x (car cursor)) (y (cadr cursor)))
                          (list (sub1 (/ x 300)) (- (sub1 (/ y 200))) 0)))
       cursor-position))
(define color (map (lambda (num) (/ (modulo num 61) 60)) num-frames))

(define scene
  (map
   (lambda (angle square-translation color)
     ;; represent scene as a thunk for now
     (lambda ()
       (gl:Clear gl:COLOR_BUFFER_BIT)
       (gl:LoadIdentity)
       (apply gl:Translatef square-translation)
       (gl:Rotatef angle 0 0 1)
       (gl:Begin gl:QUADS)
       (gl:Color3f color 0 0)
       (gl:Vertex2f -0.2 0.2)
       (gl:Color3f 0 color 0)
       (gl:Vertex2f 0.2 0.2)
       (gl:Color3f 0 0 color)
       (gl:Vertex2f 0.2 -0.2)
       (gl:Color3f color 0 color)
       (gl:Vertex2f -0.2 -0.2)
       (gl:End)))
   square-angle square-translation color))


(define scene-receiver (gochan))
(emitters-set! scene (list scene-receiver))

(with-window (600 400 "GLFW3 Test" resizable: #f swap-interval: 1)
  (start-signal-graph! scene)
  (let loop ()
    (nonblocking-swap-buffers)
    (gc #f)
    (wait-vblank)
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
