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
				   vecinos: (int ,float) Hashtbl.t;
    		   }

let tamano = ref 0
let orden = ref 0

(**funcion que inicializa la grafica. Las graficas se representan como un arreglo de referencias
  a listas. Cada entrada del arreglo representa un verice y cada lista contiene las adyacencias
  del nodo
  @param initgraf numero de vertices de la grafica
  *)  
let initgraf n =  Array.make n {id=0;nombre = "";pais="";poblacion=0;latitud=0.0;longitud=0.0;vecinos=Hashtbl.create 20}


(**
funcion para agregar una ciudad a la grafica
@param g grafica a la que se agregara la ciudad
@param v ciudad que se agregara
*)
let agrega g v = tamano:=!tamano+1;g.(!tamano)<-v

(**
Valida si es posible conectar dos nodos o si ya existe una arista que los une en esa direccion
@param ht hashtable que contiene vecinos
@param e el elemento a buscar
*)                   
let conectados g u v = Hashtbl.mem g.(u).vecinos v
                
(**
conecta dos nodos de una grafica. Al conecta los nodos i y j se agrega a la entrada i del arreglo
el par (j,p) donde p es el peso de la arista
@param a verice a conectar
@param b vertice a conectar
@param c peso de la arista que conecta a a y b
*)                
let conecta g a b c = let n = (Array.length g)-1 in
                      if (a < 0 || a > n || b < 0 || b > n)
                      	then raise(ErrorGrafica "Se ingreso un indice inexistente")
                        else
                        	let v1 = not (conectados g a b) in
                        	let v2 = not (conectados g b a) in
                        	if v1 && v2
                        	then
                        		(Hashtbl.add g.(a).vecinos b c;
                        		 Hashtbl.add g.(b).vecinos a c)
                        	else
                        		raise(ErrorGrafica "Vertices ya conectados")
                            
(**
regresa el peso de la arsita entre dos nodos a partir del indice de ambos
@param g grafica
@param u verice
@param v verice
*)
let getPeso g u v = Hashtbl.find g.(u).vecinos v

(**
funcion para obtener los vecinos de un verice
@param v verice del que se obtendran los vecinos
@return arreglo de indice de vecinos
*)
let get_vecinos v =
	let vec = Array.make (Hashtbl.length v.vecinos) 0  in
	let ind = ref 0 in
	let aux u x k valu = u.(!x)<-k;x:=!x+1 in
	Hashtbl.iter (aux vec ind) v.vecinos;
	vec


