all:
	jbuilder build @install 
test:
	jbuilder build @runtest
clean:
	rm -rf _build


.PHONY : pin
pin: 
	opam pin add caravand . -n && opam remove caravand && opam install caravand
