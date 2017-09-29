binaries = *.cmi *.cmo 
logFiles = *.log *.cache 

make: test1.ml hw1.ml 
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g hw1.ml test1.ml
clean: 
	rm $(binaries)

cleanTest: 
	rm $(logFiles)

cleanAll: clean cleanTest

