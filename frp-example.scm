(use glfw3 gl nonblocking-swap-buffers matchable)

(load "frp")

(define new-frame
  (make-primitive-signal
   (lambda (self children input)
     (for-each
      (lambda (c)
        (gochan-send
         c
         (if (equal? (gochan-receive input) 'new-frame)
             '(change new-frame)
             '(no-change new-frame))))
      children))))

(define cursor-position
  (make-primitive-signal
   (let ((state '(cursor-position 0 0)))
     (lambda (self children input)
       (let ((msg (gochan-receive input)))
         (for-each
          (lambda (c)
            (gochan-send
             c
             (match msg
               (('change ('cursor-position x y))
                (set! state (list x y))
                (list 'change state))
               (else (list 'no-change state)))))
          children))))))

;; (define clock (signal-map get-time new-frame))
;; or something like that
(define clock
  (make-registered-signal
   (list new-frame)
   (let ((state 0))
     (lambda (self children new-frame)
       (let ((msg (gochan-receive new-frame))
             (now (get-time)))
         (for-each
          (lambda (c)
            (gochan-send
             c
             (match msg
               (('change _)
                (set! state now)
                (list 'change state))
               (else
                (list 'no-change state)))))
          children))))))

(define scene
  (make-registered-signal
   (list clock cursor-position)
   (let ((state void))
     (lambda (self children clock cursor-position)
       (let ((now (gochan-receive clock))
             (pos (gochan-receive cursor-position)))
         (if (and (eq? (car now) 'no-change)
                  (eq? (car pos) 'no-change))
             (for-each (lambda (c) (gochan-send c (list 'no-change state))) children)
             (begin
               (set! state
                 ;; represent scene as a thunk for now
                 (lambda ()
                   (print (list cursor-position: (first (cadr pos)) (second (cadr pos))))
                   (gl:Clear gl:COLOR_BUFFER_BIT)
                   (gl:LoadIdentity)
                   (gl:Rotatef (* 100 (cadr now)) 0 0 1)
                   (gl:Begin gl:QUADS)
                   (gl:Vertex2f -0.2 0.2)
                   (gl:Vertex2f 0.2 0.2)
                   (gl:Vertex2f 0.2 -0.2)
                   (gl:Vertex2f -0.2 -0.2)
                   (gl:End)))
               (for-each
                (lambda (c) (gochan-send c (list 'change state)))
                children))))))))

(define scene-receiver (gochan))
(emitters-set! scene (list scene-receiver))


(cursor-position-callback
 (lambda (window x y)
   (broadcast-message! (list 'cursor-position x y))))

(with-window (600 400 "GLFW3 Test" resizable: #f)
  (start-signal-graph! scene)
  (let loop ()
    ;; (nonblocking-swap-buffers)
    ;; (gc #f)
    ;; (wait-vblank)
    (swap-buffers (window))
    (poll-events)
    (broadcast-message! 'new-frame)
    ((cadr (gochan-receive scene-receiver)))
    (unless (window-should-close (window))
      (loop))))
