all: My_Lambda

My_Lambda: My_Lambda.hs
	ghc -O2 MyLambda

clean:
	rm -f ./MyLambda
	rm -f ./Cmd.hi ./Cmd.o
	rm -f ./Eval.hi ./Eval.o
	rm -f ./Expr.hi ./Expr.o
	rm -f ./MyLambda.hi ./MyLambda.o
	rm -f ./PrettyExpr.hi ./PrettyExpr.o
	rm -f ./Subst.hi ./Subst.o
