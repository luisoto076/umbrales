open Grafica
open Conexion_db
open Solucion

let calcula_lote g t s =
	let c = ref 0 in
	let r = ref 0.0 in
	while !c < Conf.l do
		let fs = Solucion.f g s in
		Solucion.vecino s;
		let fs1 = Solucion.f g s in
		if fs1 < fs+.t
		then
			(c := !c+1;
			 r := !r+.fs1)
		else 
			(Solucion.regresa s)
	done;
	(!r/.(float_of_int Conf.l),s)

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
	let db = Conexion_db.open_db_conection "data/insumos.db" in
	let g = Grafica.initgraf 280 in
	get_rows "cities" (agrega_ciudad g)  db;
	get_rows "connections" (conecta_ciudad g) db;
	Solucion.inicializa (int_of_string Sys.argv.(1));
	let s = Solucion.genera_solucion g 10 in
	Array.iter (Printf.printf("%d ")) s;
	print_string "\n";
	Solucion.permuta s;
	Array.iter (Printf.printf("%d ")) s;
	print_string "\n";
	let par = calcula_lote g 8. s in
	Printf.printf("%f ") (fst par);
	print_string "\n"
