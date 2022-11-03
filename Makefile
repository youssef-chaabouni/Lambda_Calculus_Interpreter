all: My_Lambda

My_Lambda: ./MyLambda.hs
	ghc -O2 MyLambda -package haskeline

clean:
	rm -f ./MyLambda
	rm -f ./*.hi ./*.o