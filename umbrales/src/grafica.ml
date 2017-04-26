(**
Modulo para representar graficas de ciudades
*)

(**Exepcion para indicar errores en las operaciones de la grafica*)
exception ErrorGrafica of string

(**
record para guradar la informacion de una ciudad
*)
type ciudad = {
				   id:int;
				   nombre:string;
				   pais:string;
				   poblacion:int;
				   latitud:float;
				   longitud:float;
    		   }
    		   
type grafica = {mutable vertices:ciudad array; mutable aristas: float array array; orden: int ref}


(**funcion que inicializa la grafica. Las graficas se representan como un arreglo de referencias
  a listas. Cada entrada del arreglo representa un verice y cada lista contiene las adyacencias
  del nodo
  @param initgraf numero de vertices de la grafica
  *)  
let initgraf n = 
	let v = Array.make n {id=0;nombre = "";pais="";poblacion=0;latitud=0.0;longitud=0.0} in
	let a = Array.make_matrix n n 0. in
	{vertices=v; aristas=a; orden=ref 0}

(**
funcion para agregar una ciudad a la grafica
@param g grafica a la que se agregara la ciudad
@param v ciudad que se agregara
*)
let agrega g v = g.orden := !(g.orden) + 1; g.vertices.(!(g.orden)) <- v

(**
Valida si es posible conectar dos nodos o si ya existe una arista que los une en esa direccion
@param ht hashtable que contiene vecinos
@param e el elemento a buscar
*)                   
let conectados g u v = g.aristas.(u).(v) <> 0.

(**
regresa el peso de la arsita entre dos nodos a partir del indice de ambos
@param g grafica
@param u verice
@param v verice
*)
let getPeso g u v = g.aristas.(u).(v)
                
(**
conecta dos nodos de una grafica. Al conecta los nodos i y j se agrega a la entrada i del arreglo
el par (j,p) donde p es el peso de la arista
@param a verice a conectar
@param b vertice a conectar
@param c peso de la arista que conecta a a y b
*)                
let conecta g a b c = let n = !(g.orden) in

                      if (a < 0 || a > n || b < 0 || b > n)
                      	then raise(ErrorGrafica "Se ingreso un indice inexistente")
                        else
                        	let v1 = not (conectados g a b) in
                        	let v2 = not (conectados g b a) in
                        	if v1 && v2 then(
                        		g.aristas.(a).(b) <- c;
                        		g.aristas.(b).(a) <- c;
                        	)else
                        		raise(ErrorGrafica "Vertices ya conectados")
                            

(**
funcion para obtener los vecinos de un verice
@param v verice del que se obtendran los vecinos
@return arreglo de indice de vecinos
*)
let get_vecinos g v =
	let n = ref 0 in
	for k = 0 to !(g.orden)-1 do
		if not (g.aristas.(v).(k) = 0.) then n:= !n + 1 else () 
	done;
	let aux a i j x = 
		if not (x = 0.) then (
			a.(!j) <- !i;
			j := !j + 1
		)else();
		i:= !i + 1 
	in
	let ind1 = ref 0 in
	let ind2 = ref 0 in
	let vec = Array.make !n 0  in
	Array.iter (aux vec ind1 ind2) g.aristas.(v);
	vec


