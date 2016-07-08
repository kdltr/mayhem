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
(include "poulexagone/menus")
(include "poulexagone/draw")

(define state
  (frp:fold
   update
   (gameover 0)
   ;; initial-gamestate
   (frp:merge
    clock
    movement-keys
    spacebar
    )))

(define fps-counter
  (let ((frame-counter (frp:fold (lambda (prev _) (add1 prev)) 0 new-frame))
        (clock (frp:map (lambda (_) (get-time)) new-frame)))
   (frp:map / frame-counter clock)))


(define scene (frp:map draw state fps-counter))

(run-scene scene)

(destroy-window (window))
(exit)
