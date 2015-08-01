
open Graphics;;

set_window_title "asteroids";;
(*------------------- constantes et parametres ---------------------*)
let pi = 3.14159265359;;

(* --------------------- angle de rotation du vaisseau ------------------------- *)
let angle_rotation = pi/.10.;;


(*------------------- dimension fenetre graphique -----------------*)
let width = 1000;;
let height = 600;;


(* --------------------------- declaration des types ------------------------- *)
type point = {x:int; y:int};;
type couleur = {r:int; g:int; b:int};;
type vaisseau = {a :point; b:point; c:point; centre_ship:point; dx:point};;
type asteroid = {centre_ast:point; rayon_ast:int; dir_ast:float; couleur:couleur; speed:int};;
type projectile = {centre_proj:point; rayon_proj:int; speed_proj:point};;
type etat = {ship: vaisseau; ast:asteroid list; proj: projectile list; vies:int};;




(* --------------------------- initialisations etat ---------------------------- *)
let centre_vais = {x = width/2; y = height/2};;

let c_ship = {x = (centre_vais.x); y = (centre_vais.y)+20};;



(* --------------------- degree_to_radians ------------------------- *)
(*
   * convertion d angle de degres en radians
   * @param d : angle en degres
   * @return : angle en radians 
*)

let degree_to_radians d = (float_of_int d) *. (pi/.180.);;


(* --------------------- fonction iter ------------------------- *)
(*
   * iterer (n) fois une foncion (f) en ajoutant le resultat a la liste (a) 
   * @param n : nombre de fois qu on itère 
   * @param f : fonction a appliquer sur les elements 
   * @param a : liste d elements 
   * @return : liste contenant le reslultat de l iteration 
*)
let rec iter n f a =
if n = 0 then a else f (iter (n-1) f a);;



(* --------------------- generer couleur ------------------------- *)
(*
   * generer des couleurs aleatoires  
   * @param () : cette fonction prend en parametre un type unit 
   * @return : retourne un objet de type couleur d intensité  RGB aleatoire
*)
let gen_couleur =  function () -> {r = (Random.int 256); g = (Random.int 256); b = (Random.int 256)};; 




(* --------------------- generer un asteroide ------------------------- *)    
(*
   * generer un asteroide aleatoire (rayon, direction, couleur et vitesse aleatoires)
   * @param x : liste ou on va ajouter l asteroid
   * @return : retourne la liste x  
*)

let ajout_asteroid x = let r1 =  (Random.int width) in
                       let r2 =  (Random.int height) in
                       if ((r1 < width/4) || (r1 >(3*width)/4) ||  (r2 < height/4) || (r2 >(3*height)/4)) then 
                         let direction = (degree_to_radians ((Random.int 360)+1)) in 
			 let vitesse = ((Random.int 5) +3) in 
                          {centre_ast = {x=r1;y=r2}; 
			   rayon_ast = ((Random.int 2)+1)*20; 
			   dir_ast= direction; 
			   couleur= gen_couleur() ; 
			   speed =vitesse}::x
                       else x;;




(* --------------------- fragment_asteroid ------------------------- *)
(*
   * generer astroide cas de collisition d'un grand asteroide avec un projectile 
   * @param point : position ou on va generer l'asteroide
   * @param taille : taille de asteroide generé
   * @param couleur : couleur de asteroide genere
   * @return : returne un nouveau asteroide de position (point), de taille (taille) et de couleur (couleur)
*)
let fragement_asteroid point  taille couleur= 
                         let direction = (degree_to_radians ((Random.int 360)+1)) in 
			 let vitesse = ((Random.int 5) +1) in 
                          {centre_ast = {x=point.x ;y= point.y}; rayon_ast = taille ; dir_ast= direction; couleur= couleur; speed =vitesse};;



       
(* --------------------- generer les asteroides ------------------------- *)
(*
   * generer 20 asteroides 
   * @param l : liste ou on sauvegarde les asteroides generés 
   * @return : liste d'asteroides 
*)
let gen_asteroids l = iter 20 ajout_asteroid  l;;  





(* --------------------- convert_to_int------------------------- *)
(*
   * cenvertir un nombre reel en nombre entier  
   * @param f : nombre reel
   * @return : l'arrondi du nombre f
*)
let convert_to_int f = let entf = int_of_float f in
		       if f-. (float_of_int entf) >=0.5 then entf + 1 
		       else entf;;





(* --------------------- rotation ------------------------- *)
(*
   * rotation d un point par rapport a un autre avec un angle donné en parametre 
   * @param p : point pour lequel on calcule la rotation
   * @param c : centre autour du quel on effectue la rotation
   * @param angle : angle de rotation
   * @return : point resultant de la rotation de p autour de c 
*)
let rotation p c angle = let ss = sin(angle) in
			  let  cs = cos(angle) in
			  let  a =  float_of_int(p.x-c.x) in
			  let b = float_of_int(p.y-c.y) in
			  { x =convert_to_int (float_of_int (c.x) +. a*. cs -. b*. ss); 
			    y =convert_to_int (float_of_int (c.y) +. a*. ss +. b*. cs)};;




(*------------------------- init_etat ------------------------------- *)

(*
   * initialise l etat initial du jeu
   * @return : un etat avec le vaisseau placé au mileu de l ecran, la liste de 20 asteroides genéres avec gen_asteroids,
   *			une liste de projectiles vides et un nombre de vies initialisé a 3
*)

let init_etat () = {ship = {a = rotation c_ship centre_vais (3.*.pi/.4.);
			    b = rotation c_ship centre_vais (5.*.pi/.4.); 
			    c = c_ship; 
			    centre_ship = centre_vais; dx = {x=0; y=0}};
		    ast= gen_asteroids []; 
		    proj = []; vies = 3};; 






(* --------------------- reapparition du  vaisseau dans en cas ou il sort de la fenetre ------------------------- *)

(*
   * Assure que le vaisseau ne sort pas de l'écran de jeu
   * @param ship: vaisseau 
   * @return : le vaisseau ship avec des coordonnées comprises entre height et weight
*)
let inside_window ship = 
  let xc=  (ship.c.x-ship.centre_ship.x) in
  let yc=  (ship.c.y-ship.centre_ship.y) in
		if (ship.centre_ship.x > width) then 
		{ship with a = rotation ({x = xc; y = ship.c.y}) ({ x = 0; y = ship.centre_ship.y}) (5.*.pi/.4.);
			        b = rotation ({x = xc; y = ship.c.y}) ({ x = 0; y = ship.centre_ship.y}) (3.*.pi/.4.); 
			        centre_ship ={ x = 0; y = ship.centre_ship.y};
			        c = {x = xc; y = ship.c.y}
			        } 
			        
		else if (ship.centre_ship.y > height) then 
		{ship with centre_ship ={ x = ship.centre_ship.x; y = 0};
			        c = {x =ship.c.x; y = yc};
					a = rotation ({x =ship.c.x; y = yc}) ({ x = ship.centre_ship.x; y = 0}) (5.*.pi/.4.);
			        b = rotation ({x =ship.c.x; y = yc}) ({ x = ship.centre_ship.x; y = 0}) (3.*.pi/.4.)			         
			        }
			    
		else if (ship.centre_ship.y <= 0) then 
		{ship with  centre_ship ={ x = ship.centre_ship.x; y = height};
					c = {x =ship.c.x; y = height+ yc};
					a = rotation ({x =ship.c.x; y = height+ yc}) ({ x = ship.centre_ship.x; y = height}) (5.*.pi/.4.);
			        b = rotation ({x =ship.c.x; y = height+ yc}) ({ x = ship.centre_ship.x; y = height}) (3.*.pi/.4.)
			        
			        }
			         
		else if (ship.centre_ship.x <= 0) then 
		{ship with centre_ship ={ x = width; y = ship.centre_ship.y};
					c = {x = width+ xc; y = ship.c.y}; 
					a = rotation ({x = width+ xc; y = ship.c.y}) ({ x = width; y = ship.centre_ship.y}) (5.*.pi/.4.);
			        b = rotation ({x = width+ xc; y = ship.c.y}) ({ x = width; y = ship.centre_ship.y}) (3.*.pi/.4.)
			        
			        } 
		else ship;;
                   
(* --------------------- acceleration_ship ------------------------- *)

(*
   *deplacement du vaisseau d'une unité selon la direction sa direction
   * @param ship : vaisseau  
   * @return :  vaisseau avec de nouvelles coordonnées
*)

let acceleration_ship ship =
  let xc= (ship.c.x-ship.centre_ship.x)/2 in
  let yc= (ship.c.y-ship.centre_ship.y)/2 in
                  {ship with a= {x=(ship.a.x+xc);  y= (ship.a.y+yc) }; 
                   b= {x=(ship.b.x+xc);  y= (ship.b.y+yc)}; 
               	   c= {x=(ship.c.x+xc);   y= (ship.c.y+yc)};
				   centre_ship = {x=(ship.centre_ship.x+xc);   y= (ship.centre_ship.y+yc)}};;


(* ------------------------ acceleration --------------------------------- *)	
(*
   * Calcule la nouvelle position du vaisseau lorsque le joueur appuie sur la touche d'acceleration
   * @param etat : etat du jeu   
   * @return : etat avec les nouvelles coordonnées du vaisseau 
*)	       
                                
let  acceleration etat = 
  let xc= (etat.ship.c.x-etat.ship.centre_ship.x)/4 in
  let yc= (etat.ship.c.y-etat.ship.centre_ship.y)/4 in
 {etat with ship = acceleration_ship (inside_window  {etat.ship with dx = {x=xc; y=yc}})};;


(* --------------------- mouvement ------------------------- *)
(*
   * mouvement du vaisseau avec une distance dx 
   * @param etat : ship
   * @return :ship avec les nouvelles coordonnées du vaisseau 
*)
let mouvement ship =
                  {ship with a= {x=(ship.a.x+ship.dx.x);  y= (ship.a.y+ship.dx.y) }; 
                   b= {x=(ship.b.x+ship.dx.x);  y= (ship.b.y+ship.dx.y)}; 
               	   c= {x=(ship.c.x+ship.dx.x);   y= (ship.c.y+ship.dx.y)};
				   centre_ship = {x=(ship.centre_ship.x+ship.dx.x);   y= (ship.centre_ship.y+ship.dx.y)}};;
                   
		                                       
(* ------------------------ acceleration_fluide --------------------------------- *)		                                       
(*
   * continue a faire avancer le vaisseau apres une acceleration (utilise la fonction (mouvment ship))
   * @param etat : etat du jeu
   * @return :etat avec les nouvelles coordonnées du vaisseau 
*)
let  mouvement_fluide etat = {etat with ship = inside_window (mouvement etat.ship)} ;;

                        



(* -------------------------- rotation_gauche ----------------------------------- *)
(*
   * rotation du vaisseau vers la gauche avec un angle predefini de pi/15
   * @param e : etat du jeu
   * @return : etat avec de les nouvelles coordonées du vaisseau apres une rotation_gauche 
*)
let rotation_gauche = function e -> {e with ship = {e.ship with a= rotation (e.ship.a) (e.ship.centre_ship) (angle_rotation); 
						     b= rotation (e.ship.b) (e.ship.centre_ship) (angle_rotation); 
						     c= rotation (e.ship.c) (e.ship.centre_ship) (angle_rotation); 
						     centre_ship = e.ship.centre_ship }};;


(* -------------------------- rotation_droite ------------------------------------ *)
(*
   * rotation du vaisseau vers la droite avec un angle predefini de pi/15
   * @param e : etat du jeu
   * @return : etat avec de les nouvelles coordonées du vaisseau apres une rotation_droite 
*)
let rotation_droite = function e -> {e with ship = {e.ship with  a= rotation (e.ship.a) (e.ship.centre_ship) (0.-.angle_rotation); 
						     b= rotation (e.ship.b) (e.ship.centre_ship) (0.-.angle_rotation); 
						     c= rotation (e.ship.c) (e.ship.centre_ship) (0.-.angle_rotation);  
						     centre_ship = e.ship.centre_ship}};;




(* -------------------------- tir_bis ------------------------------- *)
(*
   * fonction intermediaire pour generer un projectile
   * @param ship : vaisseau
   * @return : projectile avec ses coordonnées, son rayon et un point qui defini sa vitesse 
*)
let tir_bis ship = 
  let xc= (ship.c.x-ship.centre_ship.x) in
  let yc =(ship.c.y-ship.centre_ship.y) in
  {centre_proj={x= ship.centre_ship.x; y=ship.centre_ship.y};   
			           rayon_proj=2; speed_proj = {x = xc; y = yc}};;
	

(* -------------------------------- mouv_proj ---------------------------------- *)
(*
   * mouvement d'un projectile en ajoutant au point centre la distance speed_proj
   * @param proj : projectile
   * @return : projectile avec ses nouvelles données 
*)
let mouv_proj proj  = {proj with centre_proj={x= proj.centre_proj.x+proj.speed_proj.x; y=proj.centre_proj.y+proj.speed_proj.y}};;


(* --------------------------  -------------- *)			    
(*
   * ajout d un projectile dans la liste des projeciles
   * @param etat: etat du jeu 
   * @return : etat avec avec mise a jour de la  liste des projectiles 
*)
let tir etat = {etat with proj =(mouv_proj (tir_bis etat.ship))::etat.proj};;



(* ------------- selectionner les projectiles qui sont a l interieur de la fenentre -------------*)
(*
   * Determine si un projectile est sorti de l'écran  
   * @param p: un projectile 
   * @return : vrai si le projectile p est a l'interieur de la fenetre de jeu, faux sinon
*)
let select_proj p = let x = p.centre_proj.x in
		    let y = p.centre_proj.y in
		    x <= width && x >= 0 && y <= height && y >= 0;;




(* ------------------------ mouv_all_proj ----------------- *)

(*
   * Fonction qui gere le mouvement des projectiles
   * @param ship : vaisseau qui a effectué le tir 
   * @param projs : les projectiles poru lesquels on effectue le mouvement
   * @return : la liste des projectiles deplacés a l'aide de la fonction mouv_proj
*)
let mouv_all_proj ship projs  = List.filter select_proj  (List.map mouv_proj projs);;



(* -------------------------- affiche_ship  --------------------- *)
(*
   * Affiche le vaisseau 
   * @param e : etat pour lequel on affiche le vaisseau 
   * @return : () l'affichage du vaisseau sur l'écran de jeu
*)

let affiche_ship e =  
  draw_poly [|((e.ship.a.x),(e.ship.a.y));((e.ship.b.x),(e.ship.b.y));((e.ship.c.x),(e.ship.c.y))|];; 



(* ---------------------------- carre ------------------------------- *)
(*
   * calcul le carre d'un nombre 
   * @param x : nombre en parametre 
   * @return : retourne x*x
*)
let carre x = x*x;; 


(* ------------------------------- distance ----------------------- *)
(*
   * distance entre deux points 
   * @param p1 : premier point
   * @param p2 : deuxieme point 
   * @return : distance entre les deux points
*)
let distance p1 p2 = int_of_float (sqrt (float_of_int (carre(p1.x-p2.x)+carre(p1.y-p2.y))));;



(* -------------------- mouvement d un asteroid -------------------------- *)

(*
   * Fait déplacer un asteroide en fonction de sa vitesse
   * @param a : l'asteroide a déplacer 
   * @return : l'asteroid "a" déplacé d'une distance a.speed
*)
let mouv_asteroid a = let center = a.centre_ast in
		      let rayon = a.rayon_ast in
		      let angle = a.dir_ast in
		      let speed = a.speed in
		      { a with centre_ast= {x = if center.x-rayon> width then 0 else if center.x+rayon < 0 then width 
										else center.x + convert_to_int ((cos (angle)  *.(float_of_int speed)));
									y = if center.y-rayon > height then 0 else if center.y+rayon < 0 then height
										else center.y + convert_to_int ((sin (angle)  *.(float_of_int speed))) } };;


 (* ------------ test si deux asteroides sont de meme couleur ---------------- *)
let meme_couleur a1 a2 = a1.r = a2.r && a1.g = a2.g && a1.b = a2.b ;;




(* ------------ collision d un asteroide avec le reste des asteroides --------- *)

(*
   * Calcule la collision entre un asteroide et une liste d'asteroides
   * @param e : un astéroide
   * @param l : liste d'asteroides 
   * @return : une liste vide si il y'a eu une collision entre "e" et un élément de "l", une liste ne contenant que "e" sinon
*)
let rec collision_asteroid e l = match l with
                       []-> [e] 
		       |e1::r -> if ((distance e.centre_ast e1.centre_ast) < e.rayon_ast + e1.rayon_ast) && (meme_couleur e1.couleur e.couleur) 
			         then []
                                 else collision_asteroid e r;;




(* ----------------- collision entre tous les asteroides --------------------- *)

(*
   * Calcule la liste resultante de la collision entre deux listes d'astéroides
   * @param l1 : liste d'asteroides 
   * @param l2 : liste d'asteroides  
   * @return : liste d'asteroides contentenant les elements de l1 et l2 dont on retire tout les asteroides qui sont en collision 
*)

let rec collision_all_asteroids l1 l2 = match l1 with
                                      []-> []
                                      |e::r -> collision_asteroid e (List.filter (function x -> x <> e) l2) @ collision_all_asteroids r l2;;




(* ---------------- collision d un projectile avec les asteroides ------------ *)


(*
   * Calcule la liste resultante de la collision entre un projectile et une liste d'astéroides
   * @param p : projectile
   * @param la : liste d'asteroides  
   * @return : Si il y'a eu collision entre "p" et un élément de "la", on ajoute deux petits asteroides a "la" 
	*               et on enleve l'asteroide concerné par la collsion
   
*)

let rec collision_proj_asteroid p la = match la with
   [] ->[]
  |e::r -> if ((distance p.centre_proj e.centre_ast) < p.rayon_proj + e.rayon_ast) then
      (
      match e.rayon_ast with
	   20 -> r
       |_ -> (fragement_asteroid e.centre_ast 20 e.couleur)::(fragement_asteroid e.centre_ast 20 e.couleur) ::r
      )
      else e::collision_proj_asteroid p r;;
      
      
      
(* -------------- collisions entres projectiles et asteroides --------------- *) 

(*
   * Calcule la liste de projectiles et d'astéroides resultante de la collision des elements entre ces deux listes
   * @param lp : liste de projectiles 
   * @param la : liste d'asteroides  
   * @return : un doublet (liste de projectiles, listes d'astéroides) contenat le resulat de la collsion entre les deux listes passées en parametre
*)

let rec collision_all_proj_asteroid lp la = match lp with  
    [] -> (lp,la) 
    |e::r -> let col = (collision_proj_asteroid e la ) in
	     ((if (List.length la) = (List.length col) then e:: (fst (collision_all_proj_asteroid r col)) else (fst (collision_all_proj_asteroid r col))),
	      (snd (collision_all_proj_asteroid r col)));;


(* --------------------- collision vaisseau -------------------------------- *) 


(*
   * Dtermine si il y'a eu collision entre un astéroide et le vaisseau
   * @param s : vaisseau 
   * @param la : liste d'asteroides  
   * @return : vrai si il y'a eu collision entre un élément de "la" et le vaisseau "s", faux sinon
*)
let rec collision_ship s la = match la with
     [] -> false
     |e::r -> if (distance s.a e.centre_ast) < e.rayon_ast || (distance s.b e.centre_ast) < e.rayon_ast 
		 || (distance s.c e.centre_ast) < e.rayon_ast
	      then true else collision_ship s r ;;



(*---------------- calcul de l etat suivant, apres un pas de temps ---------- *)


(*
   * Calcule l'état suivant en utilisant les fonctions définies precedemment 
   * @param etat : l'état pou lequel on veut calculer l'état suivant 
   * @return : un état avec les mouvements des astéroides, des eventuels projectiles, et du mouvement du vaisseau
*)
let etat_suivant etat = let collisions = etat.ast in
			let proj_ast = collision_all_proj_asteroid etat.proj collisions in
			let projec = (fst proj_ast) in
			let aster = (snd proj_ast) in
						mouvement_fluide {proj = mouv_all_proj etat.ship projec ;
			             ast = if (List.map mouv_asteroid aster)=[] then gen_asteroids etat.ast else List.map mouv_asteroid aster; vies = if (collision_ship etat.ship aster) && (etat.vies >0) then (etat.vies-1) else etat.vies; 
			             ship = if collision_ship etat.ship aster then {etat.ship with a = rotation c_ship centre_vais (3.*.pi/.4.); 
																	b = rotation c_ship centre_vais (5.*.pi/.4.); 
																	c = c_ship; 
																	centre_ship = centre_vais} else etat.ship};; 







 


(* ----------------------------- affichage vaisseau ---------------------- *)

let affiche_ship e = 
       fill_poly [|((e.ship.a.x),(e.ship.a.y)); ((e.ship.b.x),(e.ship.b.y));((e.ship.c.x),(e.ship.c.y))|];;





(* --------------------------- affichage asteroid ---------------------- *)
let rec affiche_ast a = match a with
         []-> set_color white 
         |x::r-> let color = x.couleur in
		 set_color (rgb color.r color.g color.b);   fill_circle (x.centre_ast.x) (x.centre_ast.y) x.rayon_ast; affiche_ast r;;

(* --------------------------- dessine un projectile ---------------------- *)
let affiche_un_proj x = fill_circle (x.centre_proj.x) (x.centre_proj.y) x.rayon_proj;;




(* --------------------------- dessiner tous les projectiles ---------------------- *)
let rec affiche_proj a = match a with
         []-> () 
         |x::r-> affiche_un_proj x; affiche_proj r;;




(* --------------------------- affiche etat ---------------------- *)
let affiche_etat e =
 set_color black;
 fill_rect 0 0 width height;
 set_color white; 
 moveto 500 589;
 draw_string ("Vies  " ^ string_of_int e.vies);
 affiche_ast (e.ast);
 affiche_proj e.proj;
 if (e.vies <=  0) then
   ( 
     moveto 400 300;
     draw_string "Game Over :( press 'r' to restart the game";
   )
 else if (e.vies > 0 ) then  affiche_ship e 
;;





(* --- boucle d interaction --- *)

let rec boucle_interaction ref_etat =
  let status = wait_next_event [Key_pressed] in (* on attend une frappe clavier *)
  let etat = !ref_etat in (* on recupere l etat courant *)
  let nouvel_etat = (* on definit le nouvel etat... *)
    match status.key with (* ...en fonction de la touche frappee *)
    | '1' | 'j' -> rotation_gauche etat (* rotation vers la gauche *)
    | '2' | 'k' -> acceleration etat (* acceleration vers l avant *)
    | '3' | 'l' -> rotation_droite etat (* rotation vers la droite *)
    | ' ' -> tir etat (* tir d un projectile *)
    | 'q' -> print_endline "Bye bye!"; exit 0 (* on quitte le jeux *)
    | 'r' -> init_etat ()
    | _ -> etat in (* sinon, rien ne se passe *)
  ref_etat := nouvel_etat; (* on enregistre le nouvel etat *)
  boucle_interaction ref_etat;; (* on se remet en attente de frappe clavier *)

(* --- fonction principale --- *)


let main () =
  (* initialisation du generateur aleatoire *)
  Random.self_init ();
  (* initialisation de la fenetre graphique et de l'affichage *)
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  fill_rect 0 0 width height;
  set_color white;
  auto_synchronize false;
  (* initialisation de l'etat du jeu *)
  let ref_etat = ref (init_etat ()) in
  (* programmation du refraichissement periodique de l'etat du jeu et de son affichage *)
  let _ = Unix.setitimer Unix.ITIMER_REAL
    { Unix.it_interval = 0.05; (* tous les 1/20eme de seconde... *)
      Unix.it_value = 0.05 } in
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ ->
      affiche_etat !ref_etat; (* ...afficher l'etat courant... *)
      synchronize ();
      ref_etat := etat_suivant !ref_etat)); (* ...puis calculer l'etat suivant *)
  boucle_interaction ref_etat;; (* lancer la boucle d'interaction avec le joueur *)

let _ = main ();; (* demarrer le jeu *)