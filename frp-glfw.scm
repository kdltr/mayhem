(module frp-glfw
    (window-position
     window-size
     window-close
     window-focus
     window-iconify
     framebuffer-size
     mouse-button
     cursor-enter
     cursor-position
     scroll
     key
     char
     monitor
     new-frame
     run-scene)

  (import scheme chicken)
  (use glfw3 nonblocking-swap-buffers frp-lowlevel mailbox)

  (define window-position (make-primitive-signal '(0 0)))
  (window-position-callback
   (lambda (_ x y) (notify-primitive-signal! window-position (list x y))))
  
  (define window-size (make-primitive-signal '(0 0)))
  (window-size-callback
   (lambda (_ w h) (notify-primitive-signal! window-size (list w h))))
  
  (define window-close (make-primitive-signal #f))
  (window-close-callback
   (lambda (_) (notify-primitive-signal! window-close #t)))
  
  (define window-focus (make-primitive-signal #t))
  (window-focus-callback
   (lambda (_ focussed?) (notify-primitive-signal! window-focus focussed?)))
  
  (define window-iconify (make-primitive-signal #f))
  (window-iconify-callback
   (lambda (_ iconified?) (notify-primitive-signal! window-iconify iconified?)))
  
  (define framebuffer-size (make-primitive-signal '(0 0)))
  (framebuffer-size-callback
   (lambda (_ w h) (notify-primitive-signal! framebuffer-size (list w h))))

  (define mouse-button (make-primitive-signal '()))
  (mouse-button-callback
   (lambda (_ . rest) (notify-primitive-signal! mouse-button rest)))

  (define cursor-enter (make-primitive-signal #t))
  (cursor-enter-callback
   (lambda (_ entered?) (notify-primitive-signal! cursor-enter entered?)))

  (define cursor-position (make-primitive-signal '(0 0)))
  (cursor-position-callback
   (lambda (_ x y) (notify-primitive-signal! cursor-position (list x y))))

  (define scroll (make-primitive-signal '(0 0)))
  (scroll-callback
   (lambda (_ x y) (notify-primitive-signal! scroll (list x y))))

  (define key (make-primitive-signal '()))
  (key-callback
   (lambda (_ . rest) (notify-primitive-signal! key rest)))

  (define char (make-primitive-signal #\null))
  (char-callback
   (lambda (_ c) (notify-primitive-signal! char c)))

  (define monitor (make-primitive-signal '(#f #f)))
  (monitor-callback
   (lambda (m e) (notify-primitive-signal! monitor (list m e))))

  (define new-frame (make-primitive-signal #t))

  (for-each
   start-signal-graph!
   (list window-position window-size window-close window-focus window-iconify
         framebuffer-size mouse-button cursor-enter cursor-position scroll key
         char monitor new-frame))

  (define (run-scene scene)
    (define tick-receiver (make-mailbox))
    (primitive-emitters-set!
     new-frame
     (cons tick-receiver
           (primitive-emitters new-frame)))

    (define scene-receiver (make-mailbox))
    (emitters-set! scene (list scene-receiver))

    (with-window (600 400 "GLFW3 Test" resizable: #f swap-interval: 1)
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
          (loop)))))

  )

