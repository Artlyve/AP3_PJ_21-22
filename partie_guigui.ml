#load "graphics.cma";;
open Graphics;;
type couleur = Noir | Blanc ;;

type picture = couleur array array ;;
type arbre = Feuille of couleur
| Noeud of arbre * arbre * arbre * arbre ;;

let draw_picture img = 
  let size = (Array.length img) in
    resize_window size size ;
    snd (
        forloop((0, ()),
                size,
	        (function (i, ()) 
	                  -> ( snd (forloop((0, ()),
                                            size,
			                    (function (j, () ) 
			                              -> ( set_color ( match img.(i).(j) with 
			                                               | Blanc -> white 
			                                               | Noir -> black ) ;
				                           plot i j ;
				                           (j+1, ()) )
			                    )
                                 )) ;
		               (i+1, ()) )
	        )
          )
      )
;;

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

   let(i, pic) = random_img 1000 50000;;
   open_graph "";;
   draw_picture pic;;
   close_graph();;



   let a : arbre = Noeud(Feuille(Noir), Feuille(Blanc), Feuille(Noir), Feuille(Blanc));;
   



   
let image_vers_arbre
