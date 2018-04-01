all:
	jbuilder build @install @runtest
clean:
	rm -rf _build


.PHONY : pin
pin: 
	opam pin add caravand . -n && opam remove caravand && opam install caravand
