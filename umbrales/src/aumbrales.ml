open Grafica
open Conexion_db
open Solucion

let mejor_solucion = ref [|0|]
let mejor_fs = ref max_float

let calcula_lote g t s co =
	let c = ref 0 in
	let r = ref 0.0 in
	mejor_solucion := Array.copy s;
	mejor_fs := Solucion.f g s;
	while !c < Conf.l do 
		let fs = Solucion.f g s in
		Solucion.vecino s;
		let fs1 = Solucion.f g s in
		if fs1 < fs+.t
		then
			(if fs1 < !mejor_fs 
			 then
			 	(mejor_solucion := Array.copy s;
				 mejor_fs := fs1;
				 let par = factible g s in
				 if snd par then
				 	(Printf.fprintf co ("%f %d %b\n") fs1 (fst par) (snd par))
				 else ();)
			 else
			 	c := !c+1;
			 	r := !r+.fs1)
		else 
			(Solucion.regresa s)
	done;
	(!r/.(float_of_int Conf.l),s)
	
let aceptacion_por_umbrales g t s co =
	let p = ref (max_float /. 2.) in
	let tp = ref t in
	Printf.printf("%f\n%!") !tp;
	while Conf.et < !tp do
		let p1 = ref 0.0 in
		(*Printf.printf("%f\n") !mejor_fs;*) 
		while Conf.ep < (abs_float (!p -. !p1)) do
			let par = calcula_lote g !tp s co in
			p1:= !p;
			p:= fst par
		done;
		tp:=Conf.phi *. !tp;
		Printf.printf("%f\n%!") !tp		
	done 

let conecta_ciudad g row headers = 
	let u = int_of_string row.(0) in
	let v = int_of_string row.(1) in
	let d = float_of_string row.(2) in
	Grafica.conecta g u v d

let agrega_ciudad g row headers =
	let idcd = int_of_string row.(0) in
	let nodo = {
				id = idcd;
				nombre = row.(1);
				pais = row.(2);
				poblacion = int_of_string row.(3);
				latitud = float_of_string row.(4);
				longitud = float_of_string row.(5);
				vecinos = Hashtbl.create 20;
    		   } in
    Grafica.agrega g nodo
    
let () =
  	let semilla = int_of_string Sys.argv.(1) in
  	let file = "archivo"^Sys.argv.(1)^".txt" in
  	let oc = open_out file in    (* create or truncate file, return channel      *)
	let db = Conexion_db.open_db_conection "data/insumos.db" in
	let g = Grafica.initgraf 280 in
	get_rows "cities" (agrega_ciudad g)  db;
	get_rows "connections" (conecta_ciudad g) db;
	Solucion.inicializa semilla;
(*	let s = Solucion.genera_solucion g (int_of_string Sys.argv.(2)) in*)
	let s = Conf.instancia in
	Solucion.dmax g s;
	Solucion.permuta s;
	aceptacion_por_umbrales g 4. s oc;
	Array.iter (Printf.printf("%d%! ")) s;
	Printf.printf ("\n %!");
	Printf.printf("%f\n%!") !mejor_fs;
	let g = Solucion.factible g s in
	Printf.printf("%d %b\n%!") (fst g) (snd g);
	close_out oc
