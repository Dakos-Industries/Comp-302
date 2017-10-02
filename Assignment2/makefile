binaries = *.cmi *.cmo test
logFiles = *.log *.cache 

make: test1.ml sat.ml hw2_q1.ml
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g sat.ml hw2_q1.ml  test1.ml
cleanBin: 
	rm $(binaries)

cleanTest: 
	rm $(logFiles)	

clean: cleanBin cleanTest

test: make  
	./test
	
