all:
	jbuilder build @install @runtest
clean:
	rm -rf _build


.PHONY : pin
pin: 
	opam pin add letchain . -n && opam remove letchain && opam install letchain
