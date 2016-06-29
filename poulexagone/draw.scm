(define cx (/ width 2))
(define cy (/ height 2))
(define d (sqrt
            (+ (expt width 2)
               (expt height 2))))

(define (vnorm x y)
  (let ((length (sqrt (+ (expt x 2) (expt y 2)))))
    (list (/ x length) (/ y length))))

(define (vmul k v)
  (map (cut * k <>) v))

(define line-to
  (cut apply nvg:line-to! *c* <>))

(define move-to
  (cut apply nvg:move-to! *c* <>))


;; Colors

(define white (nvg:make-color-rgbf 1 1 1))
(define player-color (nvg:make-color-rgbf 0.6 0.6 1))
(define hexagon-fill-color (nvg:make-color-rgbf 0.1 0.1 0.3))
(define hexagon-stroke-color (nvg:make-color-rgbf 0 0 1))
(define background-color-1 (nvg:make-color-rgbf 0.4 0.4 0.6))
(define background-color-2 (nvg:make-color-rgbf 0.1 0.1 0.3))
(define wall-color (nvg:make-color-rgbf 1 1 0))


;; Game

(define normal-coordinates
  (let* ((rp 1) (rn (- rp))
         (sp (/ rp 2)) (sn (- sp))
         (tp (* rp (sin (/ pi 3)))) (tn (- tp)))
    (list
      (vnorm sn tn) (vnorm sp tn) (vnorm rp 0)
      (vnorm sp tp) (vnorm sn tp) (vnorm rn 0))))

(define hexagon-radius 40)

(define hexagon-coordinates
  (map (cut vmul hexagon-radius <>) normal-coordinates))

(define (apply-with p) (p normal-coordinates))

(define zones-coordinates
  (vector (map apply-with (list first second))
          (map apply-with (list second third))
          (map apply-with (list third fourth))
          (map apply-with (list fourth fifth))
          (map apply-with (list fifth sixth))
          (map apply-with (list sixth first))))

(define (draw-zone n color)
  (nvg:begin-path! *c*)
  (nvg:move-to! *c* 0 0)
  (for-each line-to (map (cut vmul d <>) (vector-ref zones-coordinates n)))
  (nvg:close-path! *c*)
  (nvg:fill-color! *c* color)
  (nvg:fill! *c*))

(define (draw-hexagon fill stroke)
  (nvg:begin-path! *c*)
  (apply nvg:move-to! *c* (car hexagon-coordinates))
  (for-each line-to (cdr hexagon-coordinates))
  (nvg:close-path! *c*)
  (nvg:fill-color! *c* fill)
  (nvg:stroke-color! *c* stroke)
  (nvg:stroke-width! *c* 3)
  (nvg:fill! *c*)
  (nvg:stroke! *c*))


(define (draw-player pos)
  (nvg:begin-path! *c*)
  (nvg:rotate! *c* pos)
  (nvg:translate! *c* 0 (- (+ hexagon-radius 5)))
  (nvg:move-to! *c* -5 0)
  (nvg:line-to! *c* 5 0)
  (nvg:line-to! *c* 0 -10)
  (nvg:close-path! *c*)
  (nvg:fill-color! *c* player-color)
  (nvg:fill! *c*))

(define (draw-wall zone position width color)
  (let ((units (vector-ref zones-coordinates zone)))
    (nvg:begin-path! *c*)
    (move-to (vmul position (car units)))
    (line-to (vmul (+ position width) (car units)))
    (line-to (vmul (+ position width) (cadr units)))
    (line-to (vmul position (cadr units)))
    (nvg:close-path! *c*)
    (nvg:fill-color! *c* color)
    (nvg:fill! *c*)))

(define (draw-background c1 c2)
  (for-each
    (lambda (n)
      (draw-zone n (if (even? n) c1 c2)))
    (iota 6)))


;; Overlay

(define (draw-overlay state fps)
  (nvg:begin-path! *c*)
  (nvg:font-size! *c* 12)
  (nvg:font-face! *c* "DejaVu")
  (nvg:fill-color! *c* white)
  ;; fps
  (nvg:text! *c* 10 10 (sprintf "~A fps" fps))
  ;; time elapsed
  (nvg:text! *c* 10 30 (sprintf "~A" (gamestate-last-update state)))
  ;; player angle
  (nvg:text! *c* 10 50
        (sprintf "~A°" (gamestate-player-angle state)))
  (nvg:text! *c* 10 70
        (sprintf "Zone ~A" (angle->zone (gamestate-player-angle state)))))

(define (draw-all state fps)
  (lambda ()
    (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:STENCIL_BUFFER_BIT))
    (nvg:begin-frame! *c* width height (/ fbwidth width))
    (nvg:save-state! *c*)

    (nvg:translate! *c* cx cy)
    ;; fancy effects
    (nvg:scale! *c* 1 0.8)
    (nvg:rotate! *c* (gamestate-board-angle state))

    (draw-background background-color-1 background-color-2)

    ;; walls
    (for-each
     (lambda (w)
       (apply (cut draw-wall <> <> <> wall-color) w))
     (gamestate-walls state))

    ;; player
    (draw-hexagon hexagon-fill-color hexagon-stroke-color)
    (draw-player (gamestate-player-angle state))

    (nvg:restore-state! *c*)

    ;; overlay
    (draw-overlay state fps)
    (nvg:end-frame! *c*)))
