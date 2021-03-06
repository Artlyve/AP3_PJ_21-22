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

   let(i, pic) = random_img 16 30;;
   open_graph "";;
   draw_picture pic;;
   close_graph();;


let image_vers_arbre k img =
  let rec iva_aux mi mxi my mxy img =
    let halfi = (mxi+mi)/2 in
    let halfiy = (mxy+my)/2 in
    if mxi - mi = 1 ||  mi - mxi = 1
    then Noeud(Feuille(img.(mi).(my)), Feuille(img.(mi).(mxi)), Feuille(img.(mxi).(mi)), Feuille(img.(mxi).(mxy)))
    else
      Noeud(iva_aux mi halfi my halfiy img, 
      iva_aux mi halfi (halfiy+1) mxy img,
      iva_aux (halfi+1) mxi my halfiy img,
      iva_aux (halfi+1) mxi (halfiy+1) mxy img
  )
    in iva_aux 0 (k-1) 0 (k-1) img
;;


image_vers_arbre 16 pic;;



let remplir_carre img k i j c =
  forloop((k-1, img), i,
    (function(l, gmi) -> 
      forloop((j, gmi), k, 
        (function(m, pic) ->
          print_int(l);

          print_int(m);
          print_newline();
          pic.(l).(m) <- c;
          (m+1,pic);
        );
      );
      (l-1, gmi);
    );
  )      
;;



let (a, p) = remplir_carre pic 4 4 0 Blanc;;
p.(1).(1);;

    type img_abr = (arbre * int);;

