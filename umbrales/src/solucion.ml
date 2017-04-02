open Grafica

let inicializa v = Random.init v

let ante1 = ref -1;
let ante2 = ref -1;


let vecino s =
	let n = (Array.length s)-1 in
	ante1 := Random.int n in
	ante2 := Random.int n in
	let aux = s.(ante1) in
	s.(ante1) <- s.(b);
	s.(ante2) <- aux
	
let dmax s = 
	let n = Array.length s in
	let max = ref 0.0
	for i = 0 to n do
		let d = Grafica.getPeso  
	done
                  
let f s g =
	let total = ref 0 in
	let n = (Array.length s)-1 in
	for i = 0 to n-1 do
		total := !total + Grafica.getPeso g s.(i) s.(i+1)
	done;
	!total/n;
            
                   
