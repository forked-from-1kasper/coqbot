build: compat
	echo Done!

coq:
	coqc core.v

compat: coq
	fsc --mlcompatibility -a logic.ml coqCompatibility.fs

run:
	fsi --mlcompatibility logic.ml io.fsx 

clean:
	rm *.vo *.glob *.ml *.mli *.dll .*.aux
