(use frp frp-glfw)

(define movement (map (cut list 'movement <>) cursor-position))
(define click (map (cut list 'click <>) mouse-button))

(define messages (merge movement click))

(define scene
  (map
   (lambda (m)
     (print m)
     void)
   messages))

(run-scene scene)
