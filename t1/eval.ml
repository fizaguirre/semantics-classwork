(* Gramatica :
t ::= true | f a l s e | i f ( t1 , t2 , t3 )
| 0 | succ ( t ) | pred ( t ) | i s z e r o ( t )
*)
type term =
TmTrue
| TmFalse
| TmIf of term * term * term
| TmZero
| TmSucc of term
| TmPred of term
| TmIsZero of term
(* Exceção a s e r ativada quando termo f o r uma FORMA NORMAL. O que , para
essa linguagem s i g n i f i c a que :
termo pode s e r um VALOR, ou
termo pode s e r um ERRO de EXECUCAO
*)
exception NoRuleApplies
(* Função a u x i l i a r para determinar se um termo é um VALOR NUMéRICO *)
let rec isnumericval t = match t with
TmZero -> true
| TmSucc( t1 ) -> isnumericval t1
| _ -> false
(* Implementação da função STEP / -> de avaliação em um passo *)
let rec step t = match t with
(* CASO IF ( t1 , t2 , t3 ) *)
TmIf (TmTrue , t2 , t3 ) ->  t2 (* regra E−IfTrue *)
| TmIf ( TmFalse , t2 , t3 ) -> t3 (* regra E−False *)
| TmIf ( t1 , t2 , t3 ) -> (* regra E− I f *)
let t1' = step t1 in
TmIf ( t1' , t2 , t3 )
(* CASO SUCC( t1 ) *)
| TmSucc( t1 ) -> (* regra E−Succ *)

let t1' = step t1 in
TmSucc( t1' )
(* CASO PRED( t1 ) *)
| TmPred(TmZero) -> (* regra E−PredZero *)
TmZero
| TmPred(TmSucc( nv1 ) ) when ( isnumericval nv1 ) -> (* regra E−PredSucc *)
nv1
| TmPred( t1 ) -> (* regra E−Pred *)
let t1'  = step t1 in
TmPred( t1' )
(* CASO ISZERO( t1 ) *)
| TmIsZero (TmZero) -> (* regra E−IsZeroZero *)
TmTrue
| TmIsZero (TmSucc( nv1 ) ) when ( isnumericval nv1 ) -> (* regra E−IsZeroSucc *)
TmFalse
| TmIsZero ( t1 ) -> (* regra E−IsZero *)
let t1' = step t1 in
TmIsZero ( t1' )
(* CASO Nenhuma regra se aplique ao termo *)
| _ ->
raise NoRuleApplies
(* Implementação de EVAL *)
let rec eval t =
try let t' = step t
in eval t'
with NoRuleApplies -> t
(* ASTs para t e s t e *)
let t1 = TmIsZero (TmZero)
let t2 = TmZero
let t3 = TmSucc(TmZero)
let t i f = TmIf ( t1 , t2 , t3 )
let t4 = TmIsZero (TmSucc(TmZero ) )
let t5 = TmIsZero ( TmFalse )
