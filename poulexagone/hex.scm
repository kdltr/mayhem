(use
  glfw3
  (prefix nanovg-gl2 nvg:)
  doodle-colors
  gl
  (prefix frp frp:)
  frp-glfw
  frp-lowlevel
  nonblocking-swap-buffers
  mailbox)

(init)

(define time (frp:map (lambda (_) (get-time)) new-frame))
(define last-time (frp:fold (lambda (old new) new) 0 time))
(define dt (frp:map - time last-time))

(make-window 680 460 "Poulexagone v2.0" resizable: #f swap-interval: 0)

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


(define tick-receiver (make-mailbox))
(primitive-emitters-set!
 new-frame
 (cons tick-receiver
       (primitive-emitters new-frame)))

(define scene-receiver (make-mailbox))
(emitters-set! scene (list scene-receiver))

(start-signal-graph! scene)

(let loop ()
  (nonblocking-swap-buffers)
  (gc #f)
  (poll-events)
  (wait-vblank)
  (poll-events)
  (notify-primitive-signal! new-frame #t)
  (let loop2 ()
    (let* ((tick-msg (mailbox-receive! tick-receiver))
           (scene-msg (mailbox-receive! scene-receiver)))
      (if (equal? '(change #t) tick-msg)
          ((cadr scene-msg)) ;; it's time to render
          (loop2))))
  (unless (window-should-close (window))
    (loop)))

(destroy-window (window))
(exit)
