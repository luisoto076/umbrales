open Random

let inicializa n = Random.init 


let vecino s =
	let n = (Array.length s)-1 in
	let a = Random.int n in
	let b = Random.int n in
	let aux = s.(a) in
	s.(a) <- s.(b);
	s.(b) <- aux
        
          
let f s g =
	let total = ref 0.0 in
	let n = (Array.length s)-1 in
	for i = 0 to n-1 do
		total := !total +. Grafica.getPeso g s.(i) s.(i+1)
	done;
	!total/.(float_of_int n);
            
                   
