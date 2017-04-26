(**Modulo principal que implementa la heristica de aceptacion por umbrales*)
open Grafica
open Conexion_db
open Solucion
open Svg
open Str

(**Variable que guarda la solucion mejor evaluada de durante todo el proceso*)
let mejor_solucion = ref [|0|]

(**Variable que guarda la mejor evaluacion de una solucion*)
let mejor_fs = ref max_float

(**diccionario que guarda las evaluaciones de soluciones aceptadas*)
(*let evals = Hashtbl.create 30*)

(**variable que guarda el numero de soluciones aceptadas*)
let aceptadas = ref 0

(**
Funcion que va construyendo los lotes y regresando el promedio de la evaluacion de la funcion de costo
*)
let calcula_lote g t s =
	let c = ref 0 in
	let r = ref 0.0 in
	mejor_solucion := Array.copy s.ruta;
	while !c < Conf.l do 
		let fs = Solucion.f g s in
		Solucion.vecino s;
		let fs1 = Solucion.f g s in
		if fs1 < fs +. t then(
			aceptadas := !aceptadas +1;
(*			 Printf.fprintf oc "E:%f\n" fs1;*)
(*			Hashtbl.add evals !aceptadas fs1;*)
			if fs1 < !mejor_fs then( 
(*				mejor_solucion := Array.copy s;*)
				mejor_fs := fs1;
			)
			else();
			c := !c+1;
			r := !r+.fs1
		)
		else 
			(Solucion.regresa s)
	done;
	!r/.(float_of_int Conf.l)

(**
funcion principal para la heristica
@param oc archivo
*)	
let aceptacion_por_umbrales g t s =
	let p = ref (max_float /. 2.) in
	let tp = ref t in
	Printf.printf("%F\n%!") !tp;
	while Conf.et < !tp do
		let p1 = ref 0.0 in 
		while (Conf.ep < (abs_float (!p -. !p1))) do
			let aux = calcula_lote g !tp s in
			p1:= !p;
			p:= aux
		done;
		tp:=Conf.phi *. !tp;
		Printf.printf("%F\n%!") !tp		
	done 

(**
funcion que se aplicara a cada valor que se regrese de la tabla "connections" y a partir de cada fila conecta dos ciudades
@param g grafica que se conectara
*)
let conecta_ciudad g row headers = 
	let u = int_of_string row.(0) in
	let v = int_of_string row.(1) in
	let d = float_of_string row.(2) in
	Grafica.conecta g u v d

(**
funcion que se aplicara a cada valor que se regrese de la tabla "cities" y a partir de cada fila de esta se crea una ciudad de la grafica
@param g grafica a la que se agregara la ciudad
@param row arreglo valores en la fila de la tabla
@param headers arreglo con el nombre de las columnas de la tabla
*)
let agrega_ciudad g row headers =
	let idcd = int_of_string row.(0) in
	let nodo = {
				id = idcd;
				nombre = row.(1);
				pais = row.(2);
				poblacion = int_of_string row.(3);
				latitud = float_of_string row.(4);
				longitud = float_of_string row.(5);
    		   } in
    Grafica.agrega g nodo

let porcentaje_aceptados g s t =
	let acept = ref 0. in
	for i = 1 to Conf.bloque do
		let fs = Solucion.f g s in
		Solucion.vecino s;
		let fs1 = Solucion.f g s in
		if fs1 < fs +.t then
			acept := !acept +. 1.
		else ()
	done;
	(!acept /. (float_of_int Conf.bloque))

let rec busqueda_binaria g s t1 t2 p =
	let tm = (t1 +. t2)/. 2. in
	if (t2 -. t1) < Conf.ep 
	then tm
	else(
		let p1 = porcentaje_aceptados g s tm in
		if (abs_float (p -. p1)) < Conf.ep 
		then 
			tm
		else(
			if p1 > p 
			then
				(busqueda_binaria g s t1 tm p)
			else 
			    (busqueda_binaria g s tm t2 p) )
		) 

let temp_inicial g s t p =
	let ti = ref t in
	let t1 = ref 0. in
	let t2 = ref 0. in
	let p1 = ref (porcentaje_aceptados g s t) in
	if (abs_float (p -. !p1)) < Conf.ep
	then
		t
	else(
		if !p1 < p then (
			while !p1 < p do
				ti := 2. *. !ti;
				p1 := porcentaje_aceptados g s !ti			
			done;
			t1 := !ti /. 2.;
			t2 := !ti 
		) else(
			while !p1 > p do
				ti:= !ti /. 2.;
				p1:= porcentaje_aceptados g s !ti	
			done;
			t1 := !ti;
			t2 := !ti *. 2. 
		);
		(busqueda_binaria g s !t1 !t2 p)
	)


(**
	funcion que realiza el preproceso para la heristica y registra los resultados
	@param g la grafica de ciudades
	@param s la solucion construida a partir del archivo que se paso como argumento
	@param semilla valor para inicializar el generador de numeros aleatorios
*)    
let umbrales g s semilla =
	Solucion.inicializa semilla;
	Solucion.permuta s;
	let t_inicial = temp_inicial g s Conf.tem_inicial 0.95 in
(*	let t_inicial = Conf.tem_inicial in*)
	let oc = open_out (Conf.exp^"/funcion"^(string_of_int semilla)^".txt")  in
	Solucion.inicializa semilla;
	aceptacion_por_umbrales g t_inicial s;
	Printf.fprintf oc ("semilla: %d\n") semilla;    
	Array.iter (Printf.fprintf oc ("%d;%!")) !mejor_solucion;
	Printf.fprintf  oc ("\n %!");
	Printf.fprintf oc ("%F\n%!") !mejor_fs;
	let g = Solucion.factible g s in
	Printf.fprintf oc ("desconexiones: %d\nfactible: %b\n Aceptadas: %d\n%!") (fst g) (snd g) !aceptadas;
	close_out oc
	
	
(**
	Funcion que a partir de la base de datos predefinida construye la grafica de ciudades
	@return la grafica de ciudades para el problema tsp
*)	
let construye_grafica () =
	let db = Conexion_db.open_db_conection "data/insumos.db" in
	let g = Grafica.initgraf 280 in
	get_rows "cities" (agrega_ciudad g)  db;
	get_rows "connections" (conecta_ciudad g) db;
	Conexion_db.close_db_conection db;
	g

(**
	Funcion que lee el archivo que se pasa como argumento y construye solucion inicial
	con la que se trabajara la heuristica
*)	
let lee_solucion () =
  	let ic = open_in Sys.argv.(2) in
  	try 
    	let line = input_line ic in
    	let re = Str.regexp "[ ]*,[ ]*" in
    	let lista = Str.split re line in
    	let n = List.length lista in
    	let arr = Array.make n 0 in
    	let aux arr i s = arr.(i) <- int_of_string s in 
    	List.iteri (aux arr) lista;
    	close_in ic;
    	arr                
  	with e ->                      
    	close_in_noerr ic;           (* emergency closing *)
    	raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)
                                    

	
(**
funcion principal de la aplicacion
*)
let () =
	let g = construye_grafica () in
	let semilla = int_of_string Sys.argv.(1) in
	Printf.printf ("inicio con semilla:%d\n%!") semilla;
	let s = Solucion.init g (lee_solucion ()) in
	Array.iter (Printf.printf ("%d,%!")) s.ruta;
	Printf.printf("\n");
	Printf.printf ("%F %F\n") s.max_dist_path s.avg_path;
	Printf.printf("f:%F\n") (Solucion.f g s);
(*	let s = Solucion.genera_solucion g (int_of_string Sys.argv.(2)) in*)
	umbrales g s semilla;
	Printf.printf ("termino con semilla:%d\n%!") semilla; 
(*	Svg.gfuncion evals semilla;*)
	Svg.guarda g s.ruta semilla
	

