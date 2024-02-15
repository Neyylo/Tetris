#use "AP1inter.ml" ;;

(*thomas*)

type t_param = {size_x : int ; size_y : int ; dilat : int; margin : int} ;; (*size_x -> largeur de la grille, size_y -> hauteur de la grille, dilat -> c�t� d'un carr�, margin -> �cart fen�tre-cadre noir,*)
type t_coord = {x : int; y : int} ;;
type t_point = {x : int; y : int; color : t_color} ;; (*x et y -> coordonn�es sur la grille*)

let draw_frame(p : t_param) : unit =
  (
    set_color(black) ;(*mettre en noir par d�faut*)
    fill_rect(p.margin, p.margin, p.dilat, p.dilat * (p.size_y + 1)) ; (*size_y + 1 car hauteur frame = size_y + 1*)
    fill_rect(p.margin + p.dilat, p.margin, p.dilat * p.size_x, p.dilat) ;
    fill_rect(p.margin + p.dilat * (p.size_x + 1), p.margin, p.dilat, p.dilat * (p.size_y + 1)) ; (*size_y + 1 car hauteur frame = size_y + 1*)
  ) ;;

(*Mathis*)
let draw_point(point, p : t_point * t_param ) : unit =
(
    set_color(point.color) ;
    fill_rect(p.margin + p.dilat * (point.x + 1), p.margin + p.dilat * (point.y + 1), p.dilat, p.dilat) ; (*x + 1 car dilat * (nb point avant + frame)*)
    set_color(white) ;(*cadre blanc autour du carr�*)
    draw_rect(p.margin + p.dilat * (point.x + 1), p.margin + p.dilat * (point.y + 1), p.dilat, p.dilat) ;(*Bordure blanche des carr�s*)
    set_color(black) ;(*remettre en noir par d�faut*)
) ;;

(*On repr�sente les formes par des tableaux, le dernier couple de coordonn�es est en r�alit� la size de la forme*)
let square : t_coord array = [|{x = 0; y = 0}; {x = 0; y = 1}; {x = 1; y = 0}; {x = 1; y = 1}; {x = 2; y = 2}|] ;;
let line_h : t_coord array = [|{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}; {x = 4; y = 1}|] ;;
let line_v : t_coord array = [|{x = 0; y = 0}; {x = 0; y = 1}; {x = 0; y = 2}; {x = 0; y = 3}; {x = 1; y = 4}|] ;;
let shapes : t_coord array array = [|square; line_h; line_v|] ;;

type t_cur_shape = {pos_xy : t_coord ref; color : t_color ref; shape : t_coord array} ;; (*d�finit la forme concr�te avec ses coordonn�es, sa couleurs et sa forme abstraite *)

(* Nolan*)
let draw_shape(cur_shape, param : t_cur_shape * t_param) : unit = (* d�fint la fonction draw_shape qui prends en param�tre une forme concr�te et les param�tres du jeu*)
  ((*d�finit les quatres points � dessiner, les coordonn�es sont obtenus en additionnant les coordonn�es de d�part de la forme concr�te avec les coordonn�es de chaque  point de la forme abstraite *)
    let point1 : t_point = {x = !(cur_shape.pos_xy).x + cur_shape.shape.(0).x; y = !(cur_shape.pos_xy).y + cur_shape.shape.(0).y; color = !(cur_shape.color)}
    and point2 : t_point = {x = !(cur_shape.pos_xy).x + cur_shape.shape.(1).x; y = !(cur_shape.pos_xy).y + cur_shape.shape.(1).y; color = !(cur_shape.color)}
    and point3 : t_point = {x = !(cur_shape.pos_xy).x + cur_shape.shape.(2).x; y = !(cur_shape.pos_xy).y + cur_shape.shape.(2).y; color = !(cur_shape.color)}
    and point4 : t_point = {x = !(cur_shape.pos_xy).x + cur_shape.shape.(3).x; y = !(cur_shape.pos_xy).y + cur_shape.shape.(3).y; color = !(cur_shape.color)} in
    ((* d�ssine les quatres points d�finit juste avant*)
          draw_point (point1, param) ;
          draw_point (point2, param) ;
          draw_point (point3, param) ;
          draw_point (point4, param) ;
    ) ;
  ) ;;

(*thomas*)
let cur_shape_choice(color, param : t_color * t_param) : t_cur_shape = (*Cr�e un objet cur_shape � une position al�atoire � partir d'une forme et couleur choisie*)
  (
    Random.self_init() ;
    let shape : t_coord array = shapes.(Random.int(3)) in
    let random_pos : t_coord = {x = Random.int(param.size_x - (shape.(4).x - 1)); y = (param.size_y - 1 - (shape.(4).y - 1))} in
    let cur_shape : t_cur_shape = {pos_xy = ref random_pos; color = ref color; shape = shape} in
    draw_shape(cur_shape, param) ;
    cur_shape ;
  ) ;;

let erase_shape(objet, param : t_cur_shape * t_param) : unit = (*fonction permettant d'effacer l'objet cur_shape donn� en le redessinant en blanc*)
  let l_tmp : t_color = !(objet.color) in
  (
    objet.color := white ;
    draw_shape(objet, param) ;
    objet.color := l_tmp
  ) ;;

let move_left(objet, param : t_cur_shape * t_param) : unit = (*fonction pour d�placer l'objet � gauche*)
  (if !(objet.pos_xy).x > 0 && !(objet.pos_xy).x + objet.shape.(4).x <= param.size_x
   then
     (
       erase_shape(objet, param) ;
       objet.pos_xy := {x = !(objet.pos_xy).x - 1; y = !(objet.pos_xy).y} ;
       draw_shape(objet, param)
     )
  ) ;;
  
let move_right(objet, param : t_cur_shape * t_param) : unit = (*fonction pour d�placer l'objet � droite*)
  (if !(objet.pos_xy).x >= 0 && !(objet.pos_xy).x + objet.shape.(4).x < param.size_x
   then
     (
       erase_shape(objet, param) ;
       objet.pos_xy := {x = !(objet.pos_xy).x + 1; y = !(objet.pos_xy).y} ;
       draw_shape(objet, param)
     )
  ) ;;


let move_down(objet, param : t_cur_shape * t_param) : unit = (*fonction pour d�placer l'objet en bas*)
  (if !(objet.pos_xy).x >= 0 && !(objet.pos_xy).x + objet.shape.(4).x <= param.size_x
   then
     (
       erase_shape(objet, param) ;
       objet.pos_xy := {x = !(objet.pos_xy).x; y = !(objet.pos_xy).y - 1} ;
       draw_shape(objet, param)
     )
  ) ;;


let move_at_bottom(objet, param : t_cur_shape * t_param) : unit = (*fonction pour d�placer l'objet tout en bas*)
  (if !(objet.pos_xy).x >= 0 && !(objet.pos_xy).x + objet.shape.(4).x <= param.size_x
   then
     (
       erase_shape(objet, param) ;
       objet.pos_xy := {x = !(objet.pos_xy).x; y = 0} ;
       draw_shape(objet, param)
     )
  ) ;;

(*Nolan*)
let key_buffer : char ref = ref 'a' ;; (*variable qui enregistre la touche appuy�e*)
let t : float ref = ref (Unix.time()) ;; (*initialiser le temps t*)

let falling_object(objet, param : t_cur_shape * t_param) : unit = (*fonction qui permet de faire chuter un objet et de le d�placer*)
  (
    while !(objet.pos_xy).y > 0 (*boucle while qui permet de d�placer l'objet via des touches tant que y de l'objet est sup�rieur � 0 *)
    do
      if key_pressed()
      then
        (
          key_buffer := read_key();
          if !key_buffer = 'd'
          then move_left(objet, param)
          else
            if !key_buffer = 'h'
            then move_right(objet, param)
            else
              if !key_buffer = 'v'
              then move_down(objet, param)
              else
                if !key_buffer = 'c'
                then move_at_bottom(objet,param)
        );
      key_buffer := 'a'; (*r�initialise la touche de d�placement*)
      if Unix.time() -. !t > 1. (*ins�rer mot*)
      then (move_down(objet, param) ;
            t := Unix.time();(*r�initialise le temps 't'*)
           )
    done ;
  ) ;;

