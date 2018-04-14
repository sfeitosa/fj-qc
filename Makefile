default: run

run:
	ghc -o run -fhpc -fforce-recomp Tests --make -main-is "Tests" 

coverage:
	./run
	hpc markup run --exclude=Tests --exclude=FJParser

clean: 
	rm -f *.o *dyn* *.hi run run.tix *.html
