(**Modulo para operar las soluciones*)

open Grafica

(**funcion para inicializar el generador de numeros seudo aleatorios con la semilla*)
let inicializa v = Random.init v

type solucion = {mutable ruta: int array ; ant1: int ref ; ant2: int ref ; max_dist_path: float ; avg_path:float}



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
			let d = Grafica.getPeso g s.(i) s.(j) in
			if not (d = 0.0) then(
				suma:= !suma +. d;
				k:=!k+1;
				if !maxs<d then (
					maxs:=d
				)
			)else () 
		done 
	done;
	(!suma /. (float_of_int !k),!maxs)

let init g s =
	let par = init_max_avg g s in
	{ruta = s; ant1 = ref 0; ant2= ref 0; max_dist_path = snd par; avg_path = fst par}
	

(**funcion que calcula el "vecino" de una solucion
La funcion solo realiza un intercambio de valores y guarda los indices de los
valores intervambiados para poder volver a la solucion de que se partio
@param solucion a la que se le calculara el vecino*)
let vecino s =
	let n = (Array.length s.ruta) in
	s.ant1 := Random.int n; 
	s.ant2 := Random.int n; 
	let aux = s.ruta.(!(s.ant1)) in
	s.ruta.(!(s.ant1)) <- s.ruta.(!(s.ant2));
	s.ruta.(!(s.ant2)) <- aux
	
(**funcion que regresa el cambio hecho por vecino
Solo funciona para una anterior*)
let regresa s =
	let aux = s.ruta.(!(s.ant1)) in
	s.ruta.(!(s.ant1)) <- s.ruta.(!(s.ant2));
	s.ruta.(!(s.ant2)) <- aux
	
	
(**funcion para conseguir una permutacion aleatoria de la instancia inicializa
y usarla como solucion inicial de la heuristica*)	
let permuta s =
	for i = 0 to (Array.length s.ruta)/2 do
		vecino s
	done


(**funcion de costo de la heuristica*)	
let f g s =
	let total = ref 0.0 in
	let n = Array.length s.ruta in
	let prom = s.avg_path *. float_of_int (n-1) in
	for i = 0 to n-2 do
		let peso = Grafica.getPeso g s.ruta.(i) s.ruta.(i+1) in
		if not (peso = 0.) then
			total := !total +. peso 
		else
			total:=!total +. (s.max_dist_path *. Conf.c);
	done;
	!total/.prom

(**funcion para obtendran una instancia aleatoria que tenga una solucion almenos*)
let genera_solucion g n =
	let s = Array.make n 0 in
	let v = ref ((Random.int 277)+1) in
	let visit = Array.make 278 false in
	let i = ref 0 in
	while !i < n do
		let vecinos = Grafica.get_vecinos g !v in
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
	let n = Array.length s.ruta in
	let desc = ref 0 in
	let b = ref true in
	for i = 0 to n-2 do
		if (Grafica.conectados g s.ruta.(i) s.ruta.(i+1)) then () else (b:=false;desc:=!desc+1)
	done;
	(!desc,!b)


                   
