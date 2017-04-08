(**Modulo para operar las soluciones*)

open Grafica

(**funcion para inicializar el generador de numeros seudo aleatorios con la semilla*)
let inicializa v = Random.init v

(**variable que guarda la primera posicion del cambio anterior que se genero
en la funcion vecino*)
let ant1 = ref 0

(**variable que guarda la segunda posicion del cambio anterior que se genero
en la funcion vecino*)
let ant2 = ref 0

(**variable que guarda la maxima distancia entre dos ciudades
Se debe inicializar con la funcion init_max_avg *)
let max_dist_path = ref 0.

(**variable que guarda la distancia promedio entre dos ciudades
Se debe inicializar con la funcion init_max_avg *)
let avg_path = ref 0.

(**funcion que calcula el "vecino" de una solucion
La funcion solo realiza un intercambio de valores y guarda los indices de los
valores intervambiados para poder volver a la solucion de que se partio
@param solucion a la que se le calculara el vecino*)
let vecino s =
	let n = (Array.length s)-1 in
	ant1 := Random.int n; 
	ant2 := Random.int n; 
	let aux = s.(!ant1) in
	s.(!ant1) <- s.(!ant2);
	s.(!ant2) <- aux
	
(**funcion que regresa el cambio hecho por vecino
Solo funciona para una anterior*)
let regresa s =
	let aux = s.(!ant1) in
	s.(!ant1) <- s.(!ant2);
	s.(!ant2) <- aux
	
	
(**funcion para conseguir una permutacion aleatoria de la instancia inicializa
y usarla como solucion inicial de la heuristica*)	
let permuta s =
	for i = 0 to (Array.length s)-1 do
		vecino s
	done

(**
Funcion que inicializa la distancia promedio y distancia maxima entre dos ciudades
en la solucion
*)	
let init_max_avg g s = 
	let n = Array.length s in
	let maxs = ref 0.0 in
	let suma = ref 0.0 in
	let k = ref 0 in
	for i = 0 to n-2 do
		for j = i+1 to n-1 do
			try
				let d = Grafica.getPeso g s.(i) s.(j) in
				suma:=!suma +. d;
				k:=!k+1; 
				if !maxs<d then maxs:=d else () 
			with
				Not_found -> ()
		done 
	done;
	avg_path:= !suma /. (float_of_int !k);
	max_dist_path:=!maxs

(**funcion de costo de la heuristica*)	
let f g s =
	let total = ref 0.0 in
	let n = Array.length s in
	let prom = !avg_path *. float_of_int(n-1) in
	for i = 0 to n-2 do
		try
			total := !total +. Grafica.getPeso g s.(i) s.(i+1)
		with
			Not_found -> total:=!total +. (!max_dist_path *. Conf.c)
	done;
	!total/.prom

(**funcion para obtendran una instancia aleatoria que tenga una solucion almenos*)
let genera_solucion g n =
	let s = Array.make n 0 in
	let v = ref ((Random.int 277)+1) in
	let visit = Array.make 278 false in
	let i = ref 0 in
	while !i < n do
		let vecinos = Grafica.get_vecinos g.(!v) in
		let j = ref 0 in
		while (!j<(Array.length vecinos) && visit.(vecinos.(!j))) do
			j:=!j+1
		done;
		v := vecinos.(!j);
		visit.(!v) <- true;
		s.(!i) <- !v;
		i:=!i+1;
	done;
	s

(**funcion que indica si una solucion es factible y el numero de desconexiones 
presenta*)
let factible g s =
	let n = Array.length s in
	let desc = ref 0 in
	let b = ref true in
	for i = 0 to n-2 do
		if (Grafica.conectados g s.(i) s.(i+1)) then () else (b:=false;desc:=!desc+1)
	done;
	(!desc,!b)


                   
