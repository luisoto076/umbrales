open Grafica

let inicializa v = Random.init v
let ant1 = ref 0
let ant2 = ref 0
let max_dist_path = ref 0.

let vecino s =
	let n = (Array.length s)-1 in
	ant1 := Random.int n; 
	ant2 := Random.int n; 
	let aux = s.(!ant1) in
	s.(!ant1) <- s.(!ant2);
	s.(!ant2) <- aux
	
let regresa s =
	let aux = s.(!ant1) in
	s.(!ant1) <- s.(!ant2);
	s.(!ant2) <- aux
	
let permuta s =
	for i = 0 to (Array.length s)-1 do
		vecino s
	done
	
let dmax g s = 
	let n = Array.length s in
	let maxs = ref 0.0 in
	for i = 0 to n-2 do
		for j = 1 to n-1 do
			try
				let d = Grafica.getPeso g s.(i) s.(j) in
				if !maxs<d then maxs:=d else () 
			with
				Not_found -> ()
		done 
	done;
	max_dist_path:=!maxs

let avg g s = 
	let n = Array.length s in
	let prom = ref 0.0 in
    for i = 0 to n-2 do
    	try
			let d = Grafica.getPeso g s.(i) s.(i+1) in
			prom:=!prom+.d 
		with
			Not_found -> ()
	done;
	!prom/.(float_of_int (n-1))
	
let f g s =
	let total = ref 0.0 in
	let n = (Array.length s)-1 in
	let prom = (avg g s)*.float_of_int(n-1) in
	for i = 0 to n-1 do
		try
			total := !total +. Grafica.getPeso g s.(i) s.(i+1)
		with
			Not_found -> total:=!total +. (!max_dist_path*.Conf.c)
	done;
	!total/.prom

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


let factible g s =
	let n = Array.length s in
	let desc = ref 0 in
	let b = ref true in
	for i = 0 to n-2 do
		if (Grafica.conectados g s.(i) s.(i+1)) then () else (b:=false;desc:=!desc+1)
	done;
	(!desc,!b)


                   
