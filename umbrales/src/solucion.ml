open Grafica

let inicializa v = Random.init v
let ant1 = ref 0
let ant2 = ref 0

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
	let max = ref 0.0 in
	for i = 0 to n-2 do
		try
			let d = Grafica.getPeso g s.(i) s.(i+1) in
			if !max<d then max:=d else () 
		with
			Not_found -> () 
	done;
	!max

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
			Not_found -> total:=!total +. ((dmax g s)*.Conf.c)
	done;
	!total/.prom

let genera_solucion g n =
	let s = Array.make 10 0 in
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

                   
