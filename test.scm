(use srfi-18)
(use glfw3 gl posix)

(foreign-declare #<<EOF
#include <errno.h>
#include <GLFW/glfw3.h>

static int a2b_read = -1;
static int a2b_write = -1;
static int b2a_read = -1;
static int b2a_write = -1;

static GLFWwindow* window = NULL;
static pthread_t pthread;

static void* start_thread(void* args) {
  char buf[1] = {0};
  close(a2b_write);
  close(b2a_read);
  while (1) {
    printf ("THREAD: waiting message\n");
    if (read(a2b_read, buf, 1) == -1) {
        err(1, NULL);
    }
    printf ("THREAD: swapping buffers\n");
    glfwMakeContextCurrent(window);
    glfwSwapBuffers(window);
    glfwMakeContextCurrent(NULL);
    printf ("THREAD: sending message\n");
    send(b2a_write, buf, 1);
  }
}

EOF
)

(define start-thread!
  (foreign-lambda* int ()
    "C_return(pthread_create(&pthread, NULL, start_thread, NULL));"))

(define register-window!
  (foreign-lambda* void ((c-pointer w))
    "window = (GLFWwindow*) w;"))

(define (external-swap-buffers)
  (signal-swapper-thread!)
  (gc #f)
  (wait-swapper-thread!))

(define (signal-swapper-thread!)
  (print "MAIN: sending message")
  (write-char #\null a2b-write-port))

(define (wait-swapper-thread!)
  (print "MAIN: waiting message")
  (read-char b2a-read-port))

(define-values (a2b-read a2b-write) (create-pipe))
(define-values (b2a-read b2a-write) (create-pipe))

((foreign-lambda* void ((int a) (int b) (int c) (int d))
   "a2b_read = a; a2b_write = b; b2a_read = c; b2a_write = d;")
 a2b-read a2b-write b2a-read b2a-write)

(thread-start! (lambda () (print "BUSY: hello") (thread-sleep! 1)))
(start-thread!)

(file-close a2b-read)
(file-close b2a-write)

(define a2b-write-port (open-output-file* a2b-write))
(define b2a-read-port (open-input-file* b2a-read))

(with-window (600 400 "GLFW3 Test" resizable: #f)
  (register-window! (window))
  (let loop ()
    (poll-events)
    (gl:Clear gl:COLOR_BUFFER_BIT)
    (gl:LoadIdentity)
    (gl:Rotatef (* 100 (get-time)) 0 0 1)
    (gl:Begin gl:TRIANGLES)
    (gl:Vertex2f 0 0.2)
    (gl:Vertex2f 0.2 -0.2)
    (gl:Vertex2f -0.2 -0.2)
    (gl:End)
    ;;(swap-buffers (window))
    (let ((ctx (window)))
      (make-context-current #f)
      (print "MAIN: calling swapper")
      (external-swap-buffers)
      (print "MAIN: returning to loop")
      (make-context-current ctx))
    (unless (window-should-close (window))
      (loop))))
