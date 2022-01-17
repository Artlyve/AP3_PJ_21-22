type couleur = Noir | Blanc ;;

type picture = couleur array array ;;
type arbre = Feuille of couleur
| Noeud of arbre * arbre * arbre * arbre ;;


let rec forloop (r , n , next : 'a * int * ( ' a -> 'a )) : 'a =
  if n = 0 then r
  else forloop ( next ( r ) , n -1 , next )
  ;;

let is_puiss_2(n : int) : bool =
  n mod 2 = 0
  ;;

  is_puiss_2(100);;

  let random_img size n =
    let x : int ref = ref 0 and y : int ref = ref 0 in
    let pic : picture = Array.make_matrix size size Blanc in
    forloop((0, pic), n, 
      (function (i, p)  -> 
        x := Random.int size;
        y := Random.int size;
        p.(!x).(!y) <- Noir;
        (i+1, p)
      ));
       
    ;;

    random_img 10 25;;
    
    Random.int 50;;

    let c : picture = Array.make_matrix 10 10 Noir;;
c.(0).(0) <- Blanc;;
c;;
let p = [|[|Noir;Noir|];[|Blanc;Noir|]|];;
   
