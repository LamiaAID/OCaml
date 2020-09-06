type eb = V of int | TRUE | FALSE | AND of eb * eb | OR of eb * eb | XOR of eb * eb | NOT of eb 

type eq = EQ of eb * eb;;

(* cette fonction filtre les doublons d'une liste déjà triée *)

let rec filtrer l = match l with
	| [] -> []
	| a::[] -> [a]
	| a::l -> if (a==(List.hd l)) 
			then (filtrer (l)) 
			else ([a]@(filtrer l));;

(* union partiel car ce n'est pas vraiment union, mais plutôt une concaténation de deux listes triées auquel on n'ajoute pas de doublon. La liste en sortie est triée *)
			
let rec unionPartiel a b = match a with
	| [] ->  filtrer b;
	| (d1::l1) -> match b with
		| [] -> filtrer (d1::l1)
		| (d2::l2) -> if (d1 < d2) then (d1)::(unionPartiel l1 (d2::l2))
			else if (d1 == d2) then (unionPartiel l1 (d2::l2))
			else d2::(unionPartiel (d1::l1) (l2));;

(* Determiner les variables d'une expression *)			
			
let rec determinExp eb = match eb with
    | V i -> [(i)]
	| TRUE -> []
    | FALSE -> []
    | AND(a,b) -> unionPartiel (determinExp a) (determinExp b)
    | OR(a,b) -> unionPartiel (determinExp a) (determinExp b)
    | XOR(a,b) -> unionPartiel (determinExp a) (determinExp b)
    | NOT e -> determinExp e;;
	
(* Determiner les variables d'une equation *)
	
let rec determinEq eq = match eq with
	| EQ(eb1,eb2) -> unionPartiel (determinExp eb1) (determinExp eb2);;
	
(* Première fonction demandée dans le sujet *)

let rec determin (a::l) = match l with
	| [] -> determinEq a
	| l -> unionPartiel (determinEq a) (determin l);;
	
(* cette fonction ajoute une variable dans un environnement (qui est sous forme de liste )*)
	
let rec genAux e (a::l) = match l with
	| [] -> [e::a]
	| l -> (e :: a) :: (genAux e l);;
	
(* deuxième fonction demandée dans le sujet*)
	
let rec gen (a::l) = match l with
	| [] -> [[a, true];[a, false]]
	| l -> (genAux (a, true) (gen l)) @ (genAux (a, false) (gen l));;
	
(* verifie l'existance d'une variable dans l'environnement *)
	
let rec existance a n = match a with
    | [] -> false
    | (i, b)::[] -> if i = n then true
                             else false
    | (i, b)::l -> if i = n then true
                             else existance l n;;

(* trouve la valeur associée a la variable dans l'environnement *)
							 
let rec found a n = match a with
    |(i, b)::[] -> b
    |(i, b)::l -> if(i = n) then b
                            else found l n;;
							
(* troisième fonction demandée*)	
						
let rec eval eb e = match eb with
	| TRUE -> true
	| FALSE -> false
	| V i -> if (existance e i) then (found e i)
                                  else false;
	| AND(a,b) -> (eval a e)&&(eval b e)
	| OR(a,b) -> (eval a e)||(eval b e)
	| XOR(a,b) -> if((eval a e) == (eval b e)) then false else true
	| NOT a -> not (eval a e);;
	
(* ________________________________________________ *)
	
(* fonction bonus qui dit si une equation est valide pour un environnement donné ; si on a "a = b" alors eval a doit être équivalent à eval b *)
	
let evalEq (EQ(a,b)) e = if(eval a e == eval b e) then true else false;;
	
(* fonction bonus qui dit si un systeme est valide pour un environnement donné ; chaque equation composant le systeme doit être valide *)
	
let rec evalSyst (a::l) e = match l with 
	| [] -> (evalEq a e)
	| l -> (evalEq a e) && (evalSyst l e);;
	
(* fonction bonus qui filtre les environnements valides ; un environnement est valide si il est valide pour chaque equation du systeme *)
	
let rec getEnvValide syst (e::l) = match l with
	| [] -> if (evalSyst syst e) then [e] else []
	| _ -> if (evalSyst syst e) then ([e]@(getEnvValide syst l)) else (getEnvValide syst l);;
	

(* fonction bonus qui prend un systeme et retourne les environnements valides *)
	
let solutions syst = if (syst == []) then [] else getEnvValide syst (gen (determin syst));;

	
(* ________________________________________________ *)

	
let equation1 = EQ(OR(V 1, V 2), TRUE);;
let equation2 = EQ(XOR(V 1, V 3), V 2);;
let equation3 = EQ(NOT(AND(V 1, (AND(V 2, V 3)))), TRUE);;
let syst1 = (equation1)::(equation2)::(equation3)::[];;

let equation4 = EQ(XOR(V 1, V 2), FALSE);;

	
(* ________________________________________________ *)
				
let test1 = determin syst1;;
let test2 = gen test1;;
let test3 = (List.nth test2 0);;
let test4 = evalEq equation1 test3;;
let test5 = evalSyst syst1 test3;;
let test6 = getEnvValide syst1 test2;;
let test7 = solutions syst1;;
(* ci-dessus l'exemple du sujet avec une petite erreur, en effet [V1 = TRUE; V2 = FALSE; V3 = TRUE] est une solution *)

(* le seul test qu'on a pas avec l'exemple c'est celui qu'une equation avec FALSE dedans... Le voici *)
let test8 = solutions (equation4::[]);;


(* ________________________________________________ *)

(* les tests pour nous rassurer sur le bon fonctionnement de filtrer et unionPartiel *)

let test1 = filtrer [];;
let test2 = filtrer [1];;
let test3 = filtrer [1;2;3];;
let test4 = filtrer [1;1;2;3;3;3;4;4;4;6;6;6;9];;

let test5 = unionPartiel [] [];;
let test6 = unionPartiel [1] [];;
let test7 = unionPartiel [] [2];;
let test8 = unionPartiel [1;2;3] [1;2;3];;
let test9 = unionPartiel [1;3;5] [2;4];;
let test10 = unionPartiel [1;3;4;7;9;11] [2;2;3;4;5;5;6;6;6;7;7;11;13];;
