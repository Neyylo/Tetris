#use "tetris_v5.ml"

(*Nolan*)
let param : t_param = {size_x = 10; size_y = 20; dilat = 30; margin = 100} ;;(*valeur x et y du sujet*)
open_graph((param.size_x + 2) * param.dilat + param.margin * 2, (param.size_y + 1) * param.dilat + param.margin * 2) ;;(*taille de la fen�tre adapt�e par rapport � size_x et size_y et dilat*)
draw_frame(param) ;;

for i=1 to 5
do
  let objet : t_cur_shape = cur_shape_choice(red, param) in (*objet qui apparait � une position al�atoire*)
  (
    falling_object(objet, param) ; (*il descend*)
    erase_shape(objet, param) ; (*lorsque qu'il atteint tout en bas l'objet disparait*)
  )
done ;;

for i=1 to 5
do
  let objet : t_cur_shape = cur_shape_choice(green, param) in (*idem qu'au dessus avec des objet diff�rents*)
  (
    falling_object(objet, param) ;
    erase_shape(objet, param) ;
  )
done ;;
