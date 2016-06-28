(use glfw3 frp frp-glfw)

(define movement (map (cut list 'movement <>) cursor-position))
(define click (map (cut list 'click <>) mouse-button))

(define messages (merge movement click))

(define scene
  (map
   (lambda (m)
     (print m)
     void)
   messages))

(with-window (600 400 "Merge signal test" resizable: #f swap-interval: 1)
  (run-scene scene))
