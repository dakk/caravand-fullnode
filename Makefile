all:
	jbuilder build @install @runtest
clean:
	rm -rf _build


.PHONY : pin
pin: 
	opam pin add caravan . -n && opam remove caravan && opam install caravan
