open Grafica
open Conexion_db
open Variables

let agregaCiudad g row headers =
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
    Grafica.agrega g nodo;
	print_string g.(!Grafica.tamano-1).nombre;
	print_newline ()

let () =
	let db = Conexion_db.open_db_conection "src/insumos.db" in
	let g = Grafica.initgraf 280 in
	get_rows "cities" (agregaCiudad g)  db
	
