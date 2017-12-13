(*	
	Nome = Felipe Izaguirre
	Cartao = 205984
*)


type status = Progr | Logical;;

type aexp =
	Var of string * status
|	Num of int
|	Sum of aexp * aexp
|	Mult of aexp * aexp
|	Min of aexp * aexp
|	Fat of aexp;;

type bexp =
	PBool of bool
|	POr of bexp * bexp
|	PAnd of bexp * bexp
|	PNot of bexp
|	PEq of aexp * aexp
|	PLeq of aexp * aexp
|	PUneq of aexp * aexp;;

type astn =
	ABool of bool
|	AOr of astn * astn
|	AAnd of astn * astn
|	ANot of astn
|	AImpl of astn * astn
|	AEq of aexp * aexp
|	ALeq of aexp * aexp
|	AUneq of aexp * aexp;;

type cmd =
	Skip
|	Asg of string * aexp
|	Seq of cmd * cmd
|	If of bexp * cmd * cmd
|	Wh of bexp * astn * cmd;;

let rec aexpTostr (a:aexp) = match a with
	Var(x,_) -> x
|	Num(n) -> string_of_int n
|	Sum(a1, a2) -> "(" ^ (aexpTostr a1) ^ " + " ^ (aexpTostr a2) ^ ")"
|	Mult(a1, a2) -> "(" ^ (aexpTostr a1) ^ " x " ^ (aexpTostr a2) ^ ")"
|	Min(a1, a2) -> "(" ^ (aexpTostr a1) ^ " - " ^ (aexpTostr a2) ^ ")"
|	Fat(a1) -> "(" ^ (aexpTostr a1) ^ "!)";;

let rec astnTostr (a:astn) = match a with
	ABool(true) -> "T"
|	ABool(false) -> "F"
|	AOr(f, g) -> "(" ^ (astnTostr f) ^ " or " ^ (astnTostr g) ^ ")"
|	AAnd(f, g) -> "(" ^ (astnTostr f) ^ " and " ^ (astnTostr g) ^ ")"
|	ANot(f) -> "(not " ^ (astnTostr f) ^ ")"
|	AImpl(f,g) -> "(" ^ (astnTostr f) ^ " ==> " ^ (astnTostr g) ^ ")"
|	AEq(a1, a2) -> "(" ^ (aexpTostr a1) ^ " == " ^ (aexpTostr a2) ^ ")"
|	ALeq(a1, a2) -> "(" ^ (aexpTostr a1) ^ " <= " ^ (aexpTostr a2) ^ ")"
|	AUneq(a1, a2) -> "(" ^ (aexpTostr a1) ^ " <> " ^ (aexpTostr a2) ^ ")";;

let rec bexpToastn = function
|	PBool(t) -> ABool(t)
|	POr(t1, t2) -> (AOr(bexpToastn(t1), bexpToastn(t2)))
|	PAnd(t1, t2) -> (AAnd(bexpToastn(t1), bexpToastn(t2)))
|	PNot(t) -> ANot(bexpToastn(t))
|	PEq(t1, t2) -> AEq(t1, t2)
|	PLeq(t1, t2) -> ALeq(t1, t2)
|	PUneq(t1, t2) -> AUneq(t1, t2);;

let rec asubst x (a:aexp) = function
|	Var (y, s) -> if ((x = y) && (s = Progr)) then a else Var (y,s)
|	Num(n) -> Num n
|	Sum(a1, a2) -> Sum(asubst x a a1, asubst x a a2)
|	Mult(a1, a2) -> Mult(asubst x a a1, asubst x a a2)
|	Min(a1, a2) -> Min(asubst x a a1, asubst x a a2)
|	Fat(a1) -> Fat(asubst x a a1);;

let rec subst x (a:aexp) = function
|	ABool(b) -> ABool(b)
|	AOr(t1, t2) -> AOr(subst x a t1, subst x a t2)
|	AAnd(t1, t2) -> AAnd(subst x a t1, subst x a t2)
|	ANot(t) -> ANot(subst x a t)
|	AImpl(t1, t2) -> AImpl(subst x a t1, subst x a t2)
|	AEq(t1, t2) -> AEq(asubst x a t1, asubst x a t2)
|	ALeq(t1, t2) -> ALeq(asubst x a t1, asubst x a t2)
|	AUneq(t1, t2) -> AUneq(asubst x a t1, asubst x a t2);;

let rec wpc (c:cmd) psi = match c with
	Skip -> psi
|	Asg(x,a) -> (subst x a psi)
|	Seq(c1,c2) -> (wpc c1 (wpc c2 psi))
|	If(b,c1,c2) -> AAnd(AImpl((bexpToastn b),(wpc c1 psi)), AImpl(ANot(bexpToastn b),  (wpc c2 psi)))
|	Wh(b,inv,c) -> inv;;

let rec vcg (c:cmd) psi = match c with
	Skip -> ABool(true)
|	Asg(x,a) -> ABool(true)
|	Seq(c1,c2) -> AAnd((vcg c1 (wpc c2 psi)) , (vcg c2  psi))
|	If(b,c1,c2) -> AAnd((vcg c1 psi) , (vcg c2 psi))
|	Wh(b,inv,c) -> AAnd(AAnd((vcg c inv), AImpl(AAnd(inv, (bexpToastn b)), (wpc c inv))), AImpl(AAnd(inv, ANot(bexpToastn b)), psi));;


let rec vcgen phi (c:cmd) (psi:astn) = AAnd( AImpl(phi, (wpc c psi)) , (vcg c psi));;


let fat =
Seq(Seq(Asg("y", Num 1), Asg("z", Num 0 )),
	Wh(PUneq(Var("z", Progr), Var("x", Progr)),
		AEq(Var("y", Progr), Fat(Var("z", Progr))),
		Seq(Asg("z", Sum(Var("z",Progr), Num 1)),
			Asg("y", Mult(Var("y", Progr), Var("z", Progr))))));;

let succ =
Seq(Asg("a", Sum(Var("x", Progr), Num 1)), If(PEq(Min(Var("a", Progr), Num 1), Num 0), Asg("y" , Num 1), Asg("y", Var("a", Progr))));;

let p =
If(PNot(PLeq(Var("x", Progr), Var("y", Progr))), Asg("z", Var("y", Progr)), Asg("z", Var("x", Progr)));;

let fat2 =
Seq(Asg("y", Num 1), Wh(PUneq(Var("x", Progr), Num 0), AEq(Mult(Var("y", Progr),Fat(Var("x",Progr))), Fat(Var("x0", Progr))),  Seq(Asg("y", Mult(Var("y", Progr), Var("x", Progr))), Asg("x", Min(Var("x", Progr), Num 1)))));;

(*t sum =
Seq(Asg("x", Num 0), Wh(PNot(PLeq(Var("x", Num 0))), INVARIANTE, Seq(Asg("z", Sum(Var("z", Progr), Var("x", Progr))), Asg("x", Min(Var("x", Progr), Num 1)))));;*)

let copy1 =
Seq(Seq(Asg("a", Var("x", Progr)), Asg("y", Num 0)), Wh(PUneq(Var("a", Progr), Num 0), AEq(Min(Var("x", Progr), Var("y", Progr)),Min(Var("x", Progr), Var("y", Progr))) , Seq(Asg("y", Sum(Var("y", Progr), Num 1)), Asg("a", Min(Var("a", Progr), Num 1)))));;

let mult1 =
Seq(Seq(Asg("a", Num 0), Asg("z", Num 0)), Wh(PUneq(Var("a", Progr), Var("z", Progr)), AEq(Var("z", Progr), Mult(Var("a", Progr), Var("x", Progr))) , Seq(Asg("z", Sum(Var("z", Progr), Var("x", Progr))), Asg("a", Sum(Var("a", Progr), Num 1)))));;

let mult2 =
Seq(Asg("z", Num 0), Wh(PUneq(Var("y", Progr), Num 0), AEq(Var("z", Progr), Mult(Var("x", Progr),Min(Var("y0",Progr), Var("y", Progr)))) , Seq(Asg("z", Sum(Var("z", Progr), Var("x", Progr))), Asg("a", Min(Var("y", Progr), Num 1)))));;

let downfac =
Seq(Seq(Asg("a", Var("x", Progr)), Asg("y", Num 1)), Wh(PNot(PLeq(Var("a", Progr), Num 0)), AAnd(AEq(Mult(Var("y", Progr), Fat(Var("a", Progr))), Fat(Var("x", Progr))),ANot(ALeq(Var("a", Progr), Num 0))) , Seq(Asg("y", Mult(Var("y", Progr), Var("a", Progr))), Asg("a", Min(Var("a", Progr), Num 1)))));;

print_string "Programa Teste\n";;
print_string ((astnTostr (vcgen (ALeq(Num 0, Var("x", Progr))) fat (AEq(Var("y", Progr), Fat(Var("x", Progr)))))) ^ "\n\n");;

print_string "Ex.: (1)\n";;
print_string ((astnTostr (vcgen (ABool(true)) succ (AEq(Var("y", Progr), Sum(Var("x", Progr), Num 1))))) ^ "\n\n");;

print_string "Ex.: (3)\n";;
print_string ((astnTostr (vcgen (ANot(ALeq(Var("x", Progr), Num 0))) fat (AEq(Var("y", Progr), Fat(Var("x", Progr)))))) ^ "\n\n");;

print_string "Ex.: (4)\n";;
print_string ((astnTostr (vcgen (ABool(true)) fat (AEq(Var("y", Progr), Fat(Var("x", Progr)))))) ^ "\n\n");;

(*print_string "Ex.: (5)\n";;
print_string ((astnTostr (vcgen (ANot(ALeq(Var("x", Progr), Num 0))) fat2 (AEq(Var("y", Progr), Fat(Var("x", Progr)))))) ^ "\n\n");;*)

print_string "Ex.: (6)\n";;
print_string ((astnTostr (vcgen (AAnd(AEq(Var("x", Progr), Var("x0", Progr)), ANot(ALeq(Var("x", Progr), Var("x", Progr)))))  fat2 (AEq(Var("y", Progr), Fat(Var("x0", Progr)))))) ^ "\n\n");;

print_string "Ex.: (8)\n";;
print_string ((astnTostr (vcgen (ANot(ALeq(Var("x", Progr), Num 0))) copy1 (AEq(Var("x", Progr), Var("y", Progr))))) ^ "\n\n");;

print_string "Ex.: (9)\n";;
print_string ((astnTostr (vcgen (ANot(ALeq(Var("y", Progr), Num 0))) mult1 (AEq(Var("z", Progr), Mult(Var("x", Progr), Var("y", Progr)))))) ^ "\n\n");;

print_string "Ex.: (10)\n";;
print_string ((astnTostr (vcgen (AAnd(AEq(Var("y", Progr), Var("y0", Progr)), ANot(ALeq(Var("y", Progr), Num 0)))) mult2 (AEq(Var("z", Progr), Mult(Var("x", Progr), Var("y0", Progr)))))) ^ "\n\n");;

print_string "Ex.: (11)\n";;
print_string ((astnTostr (vcgen (ANot(ALeq(Var("x", Progr), Num 0))) downfac (AEq(Var("y", Progr), Fat(Var("x", Progr)))))) ^ "\n\n");;
