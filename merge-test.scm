(use frp frp-glfw)

(define movement (map (lambda m (list 'movement m)) cursor-position))
(define click (map (lambda m (list 'click m)) mouse-button))

(define messages (merge movement click))

(define scene
  (map
   (lambda (m)
     (lambda () (print m)))
   messages))

(run-scene scene)
