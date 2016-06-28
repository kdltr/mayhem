(use
  glfw3
  gl
  (prefix nanovg-gl2 nvg:)
  (prefix frp frp:)
  frp-glfw
  defstruct
  fast-generic)

(init)
(make-window 680 460 "Poulexagone v2.0" resizable: #f swap-interval: 1)

(define-values (width height) (get-window-size (window)))
(define-values (fbwidth fbheight) (get-framebuffer-size (window)))

(define pi 3.14159265358979323846264338327)
(define *c* (nvg:create-context))
(nvg:create-font! *c* "DejaVu" "/home/kooda/.guix-profile/share/fonts/truetype/DejaVuSansMono.ttf")


(include "poulexagone/logic")
(include "poulexagone/draw")

(define-record game-state angle position)

(define state
  (frp:map
   (lambda (t dt)
     (print dt)
     (make-game-state (* 2 t) (- (/ t 10))))
   time dt))

(define scene (frp:map draw-all state))

(run-scene scene)

(destroy-window (window))
(exit)
