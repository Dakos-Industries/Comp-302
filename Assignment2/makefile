binaries = *.cmi *.cmo 
logFiles = *.log *.cache 

make: test1.ml sat.ml hw2_q1.ml
	ocamlfind ocamlc -o assignment2_TestSuite -package oUnit -linkpkg -g sat.ml hw2_q1.ml  test1.ml
cleanBin: 
	rm $(binaries)

cleanTest: 
	rm $(logFiles)	

clean: cleanBin cleanTest

test: make  
	./test
	
