CSCFLAGS=-O3

all: frp-glfw.so frp-glfw.import.so frp.so frp.import.so

clean:
	rm -vf *.so *.import.so *.import.scm

frp-glfw.import.so: frp-glfw.import.scm
	csc -d0 -s $<
frp-glfw.import.scm: frp-glfw.so
frp-glfw.so: frp-glfw.scm frp-lowlevel.import.so nonblocking-swap-buffers.import.so
	csc $(CSCFLAGS) -sJ $<

frp.import.so: frp.import.scm
	csc -d0 -s $<
frp.import.scm: frp.so
frp.so: frp.scm frp-lowlevel.import.so
	csc $(CSCFLAGS) -sJ $<

frp-lowlevel.import.so: frp-lowlevel.import.scm
	csc -d0 -s $<
frp-lowlevel.import.scm: frp-lowlevel.so
frp-lowlevel.so: frp-lowlevel.scm
	csc $(CSCFLAGS) -sJ $<

nonblocking-swap-buffers.import.so: nonblocking-swap-buffers.import.scm
	csc -d0 -s $<
nonblocking-swap-buffers.import.scm: nonblocking-swap-buffers.so
nonblocking-swap-buffers.so: nonblocking-swap-buffers.scm
	csc $(CSCFLAGS) -sJ $< -lpthread
