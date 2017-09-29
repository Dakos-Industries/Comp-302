binaries = *.cmi *.cmo 
logFiles = *.log *.cache 

make: test1.ml sat.ml 
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g sat.ml test1.ml
cleanBin: 
	rm $(binaries)

cleanTest: 
	rm $(logFiles)	

clean: cleanBin cleanTest

test: make  
	./test
	
