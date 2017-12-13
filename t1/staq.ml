(* 	Nome: Felipe Izaguirre
	Cartão : 205984 *)

(*
	** Instruções de uso **

	Para executar o programa deve ser chamado a funcao eval que recebe um programa e uma lista de termos represetando a pilha e retorna o estado final,
exemplo:
		let l = TN(1)::TN(5)::[];; (* Pilha *)
		let ta = TSeq(TN(3), TGet(TX("x"), TPlus(TPop, TX("x"))));; (* Programa TA referente a prova 1 *)
		(eval ta l);;
	Tambem eh possivel chamar a funcao (stepbystepeval ta l) que ira avaliar o programa ate nao houver regras de semantica aplicaveis imprimindo o passo a passo da avalicao na tela.

	Foi feito um mapeamento dos termos da linguagem contidos no conjunto t, descritos logo abaixo.

*)

type term =
	TN of int 			(* n *)
|	TTrue				(* true *)
|	TFalse				(* false *)
|	TX of string			(* x *)
|	TIf of term * term * term	(* if t1 then t2 else t3 *)
|	TLeq of term * term		(* t1 <= t2 *)
|	TPlus of term * term		(* t1 + t2 *)
|	TSeq of term * term		(* t1;t2 *)
|	TPop				(* pop *)
|	TGet of term * term;;		(* get x in t *)

exception NoRuleApplies;;

let rec subst x (t:term) = function
|	TN(y) -> TN(y)
|	TTrue -> TTrue
|	TFalse -> TFalse
|	TX(y) -> if (x = y) then t else TX(y)
|	TIf(t1,t2,t3) -> TIf( (subst x t t1), (subst x t t2), (subst x t t3))
|	TLeq(t1,t2) -> TLeq( (subst x t t1), (subst x t t2) )
|	TPlus(t1,t2) -> TPlus( (subst x t t1), (subst x t t2) )
|	TSeq(t1,t2) -> TSeq( (subst x t t1), (subst x t t2) )
|	TPop -> TPop
|	TGet(TX(y),t1) -> if (x = y) then TGet(TX(y),t1) else TGet(TX(y), (subst x t t1))
|	TGet(_,_) -> raise NoRuleApplies;;


let rec step (prog:term) (l: term list) = match prog with
	TIf(TTrue, t2, t3) -> (t2,l) (* E-IF2 *)
|	TIf(TFalse, t2, t3) -> (t3,l) (* E-IF3 *)
|	TIf(t1,t2,t3) -> let (t', l') = (step t1 l) in (TIf(t', t2, t3),l') (* E-IF1 *)
|	TLeq(TN(n1),TN(n2)) -> if n1 > n2 then (TFalse,l) else (TTrue,l) (* E-LEQ 3 e E-LEQ 4 *)
|	TLeq(TN(n1),t2) -> let (t2',l') = (step t2 l) in (TLeq(TN(n1), t2'),l') (* E-LEQ 2 *)
|	TLeq(t1,t2) -> let (t1', l') = (step t1 l) in (TLeq(t1', t2),l') (* E-LEQ1 *)
|	TPlus(TN(n1),TN(n2)) -> (TN(n1+n2), l) (* E-PLUS3 *)
|	TPlus(TN(n),t2) -> let (t2',l') = (step t2 l) in (TPlus(TN(n), t2'),l') (* E-PLUS 2 *)
|	TPlus(t1,t2) -> let (t1',l') = (step t1 l) in (TPlus(t1',t2),l') (* E-PLUS 1 *)
|	TSeq(TN(n),t) -> (t, TN(n)::l) (* E-SEQ 2 *)
|	TSeq(t1,t2) -> let (t1', l') = (step t1 l) in (TSeq(t1',t2),l') (* E-SEQ 1 *)
|	TPop -> ((List.hd l), List.tl l) (* E-POP *)
|	TGet(TX(x),t) -> let n = (List.hd l) in  ((subst x n t),l) (* E-GET *)
|	_ -> raise NoRuleApplies;;



let rec termTostr (t:term) = match t with
	TN(x) -> (string_of_int x)
|	TTrue -> "true"
|	TFalse -> "false"
|	TX(x) -> x
|	TIf(t1,t2,t3) -> "if " ^ (termTostr t1) ^ " then " ^ (termTostr t2) ^ " else " ^ (termTostr t3)
|	TLeq(t1,t2) -> (termTostr t1) ^ " <= " ^ (termTostr t2)
|	TPlus(t1,t2) -> (termTostr t1) ^ " + " ^ (termTostr t2)
|	TSeq(t1,t2) -> (termTostr t1) ^ " ; " ^ (termTostr t2)
|	TPop -> "pop"
|	TGet(x,t) -> "get " ^ (termTostr x) ^ " in " ^ (termTostr t);;


(* Executa o programa seguindo a semantica operacional ate que nenhuma regra se aplique *)
let rec eval (prog:term) (l: term list) = try let (prog', l') = (step prog l) in (eval prog' l')
						with NoRuleApplies -> (prog, l);;

(* Executa o programa seguindo a semantica operacional ate que nenhuma regra se aplique, exibindo o passo a passo das avaliacoes *)
let rec stepbystepeval (prog:term) (l: term list) =
	try
		let (prog', l') = (step prog l) in
		(print_string ( "< " ^ (termTostr prog) ^ " , " ^ (String.concat " " (List.map termTostr l)) ^ " >" ^ "\n")); (stepbystepeval prog' l')
	with NoRuleApplies ->  (print_string ( "< " ^ (termTostr prog) ^ " , " ^ (String.concat " " (List.map termTostr l) ^ " >")));(prog l);;

let l = TN(1)::TN(5)::[];; (* Pilha *)

let ta = TSeq(TN(3), TGet(TX("x"), TPlus(TPop, TX("x"))));; (* Programa TA referente a prova 1 *)

let tb = TIf(TLeq(TPop,TN(3)), TLeq(TGet(TX("x"),TN(4)),TX("x")), TFalse);; (* Programa TB referente a prova 1 *)

