(use nonblocking-swap-buffers glfw3 gl srfi-18)

(thread-start!
 (lambda ()
   (let loop ()
     (print "WORKER1: hello")
     (thread-sleep! 1)
     (loop))))

(thread-start!
 (lambda ()
   (thread-sleep! 0.5)
   (let loop ()
     (print "WORKER2: hello")
     (thread-sleep! 1)
     (loop))))

(with-window (600 400 "GLFW3 Test" resizable: #f)
  (let loop ()
    (poll-events)
    (gl:Clear gl:COLOR_BUFFER_BIT)
    (gl:LoadIdentity)
    (gl:Rotatef (* 100 (get-time)) 0 0 1)
    (gl:Begin gl:QUADS)
    (gl:Vertex2f -0.2 0.2)
    (gl:Vertex2f 0.2 0.2)
    (gl:Vertex2f 0.2 -0.2)
    (gl:Vertex2f -0.2 -0.2)
    (gl:End)
    (nonblocking-swap-buffers)
    (gc #f)
    (wait-vblank)
    (unless (window-should-close (window))
      (loop))))
