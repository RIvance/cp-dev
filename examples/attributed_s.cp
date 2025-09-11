--> "4 + 8 is 12"

type Eval = { eval : Int };
type Print = { print : String };
type PrintAux = { printAux : String };

type ExpSig<Exp> = {
  Lit: Int -> Exp;
  Add: Exp -> Exp -> Exp;
};

evalNum = trait implements ExpSig<Eval> => {
  (Lit     n).eval = n;
  (Add e1 e2).eval = e1.eval + e2.eval;
};

printNum = trait implements ExpSig<Print> => {
  (Lit     n).print = toString n;
  (Add e1 e2).print = "(" ++ e1.print ++ " + " ++ e2.print ++ ")";
};

printChild = trait implements ExpSig<Eval => Print> => {
  (Lit     n).print = toString n;
  (Add e1 e2).print = if e2.eval == 0 then e1.print
                      else "(" ++ e1.print ++ " + " ++ e2.print ++ ")";
};

printInh = trait implements ExpSig<Eval&Print> inherits evalNum => {
  (Lit     n).print = toString n;
  (Add e1 e2).print = if e2.eval == 0 then e1.print
                      else "(" ++ e1.print ++ " + " ++ e2.print ++ ")";
};

printSelf = trait implements ExpSig<Eval => Print> => {
         (Lit     n).print = toString n;
  [self]@(Add e1 e2).print = if self.eval == 0 then "0"
                             else "(" ++ e1.print ++ " + " ++ e2.print ++ ")";
};

printMutual = trait implements ExpSig<PrintAux => Print> => {
  (Lit     n).print = toString n;
  (Add e1 e2).print = e1.printAux ++ " + " ++ e2.printAux;
};
printAux = trait implements ExpSig<Print => PrintAux> => {
  [self]@(Lit     n).printAux = self.print;
  [self]@(Add e1 e2).printAux = "(" ++ self.print ++ ")";
};

expAdd Exp = trait [self : ExpSig<Exp>] => {
  exp = open self in Add (Lit 4) (Lit 8);
};

e = new evalNum , printMutual , printAux , expAdd @(Eval&Print&PrintAux);
e.exp.print ++ " is " ++ toString e.exp.eval
