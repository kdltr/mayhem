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

(define-generic (update (any state) (any _))
  state)

(define-generic (draw (any state) (any _))
  (print "Trying to draw an unknown state: " state)
  void)

(include "poulexagone/common")
(include "poulexagone/signals")
(include "poulexagone/logic")
(include "poulexagone/menus")
(include "poulexagone/draw")

(define state
  (frp:fold
   update
   (gameover 0 (make-board last-update: (get-time) angle: 0 pulse: 0 last-pulse: (get-time) flip: #f))
   ;; initial-gamestate
   (frp:merge
    clock
    movement-keys
    spacebar
    escape
    )))

(define fps-counter
  (let ((frame-counter (frp:fold (lambda (prev _) (add1 prev)) 0 new-frame))
        (clock (frp:map (lambda (_) (get-time)) new-frame)))
   (frp:map / frame-counter clock)))


(define scene (frp:map draw state fps-counter))

(run-scene scene)

(destroy-window (window))
(exit)
