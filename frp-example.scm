(import scheme chicken data-structures srfi-18)
(use glfw3 gl frp frp-glfw)

(define frame-time (map (lambda (_) (get-time)) new-frame))
(define num-frames (map (lambda (t) (inexact->exact (floor (* 60 t)))) frame-time))

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

(with-window (600 400 "FRP example" resizable: #f swap-interval: 1)
  (run-scene scene))
