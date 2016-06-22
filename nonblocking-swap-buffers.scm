(module nonblocking-swap-buffers
    (nonblocking-swap-buffers
     wait-vblank
     grab-context!)

  (import scheme chicken foreign)
  (use srfi-18 glfw3 posix)

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
  while (1) {

    if (read(a2b_read, buf, 1) == -1) {
        err(1, NULL);
    }

    glfwMakeContextCurrent(window);
    glfwSwapBuffers(window);
    glfwMakeContextCurrent(NULL);

    write(b2a_write, buf, 1);
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

(define nonblocking-swap-buffers)
(define grab-context!)
(let ((ctx #f))
  (set! nonblocking-swap-buffers
    (lambda ()
      (register-window! (window))
      (set! ctx (window))
      (make-context-current #f)
      (write-char #\null a2b-write-port)
      (flush-output a2b-write-port)))
  (set! grab-context!
    (lambda ()
      (make-context-current ctx)
      (set! ctx #f))))

(define (wait-vblank)
  (thread-wait-for-i/o! b2a-read #:input)
  (read-char b2a-read-port))

(define-values (a2b-read a2b-write) (create-pipe))
(define-values (b2a-read b2a-write) (create-pipe))

((foreign-lambda* void ((int a) (int b) (int c) (int d))
   "a2b_read = a; a2b_write = b; b2a_read = c; b2a_write = d;")
 a2b-read a2b-write b2a-read b2a-write)

(start-thread!)

(define a2b-write-port (open-output-file* a2b-write))
(define b2a-read-port (open-input-file* b2a-read))

)
