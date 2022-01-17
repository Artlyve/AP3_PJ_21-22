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

  let random_img(size, n : int * int) : picture =
    let x : int ref = ref 0 and y : int ref = ref 0 in
    if(is_puiss_2(size))
    then let pic : picture ref = ref Array.make_matrix size size in
    forloop((0, !pic), n, 
      (function (i, p)  -> 
        x := Random.int size;
        y := Random.int size;
        (!pic).(x).(y) <- Noir;
        ));
        
    ;;

    Random.int 50;;