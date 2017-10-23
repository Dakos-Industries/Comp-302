binaries = *.cmi *.cmo test
logFiles = *.log *.cache 

make: test1.ml hw3.ml
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g hw3.ml  test1.ml
cleanBin: 
	rm $(binaries)

cleanTest: 
	rm $(logFiles)	

clean: cleanBin cleanTest

test: make  
	./test
	
