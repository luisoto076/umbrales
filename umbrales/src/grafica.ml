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

let agrega g v = g.(!tamano)<-v;tamano:=!tamano+1

(**
Valida si es posible conectar dos nodos o si ya existe una arista que los une en esa direccion
@param ht hashtable que contiene vecinos
@param e el elemento a buscar
*)                   
let valida ht e = try 
                     let _ = Hashtbl.find ht e in false
                  with 
                     Not_found -> true
                
(**
conecta dos nodos de una grafica. Al conecta los nodos i y j se agrega a la entrada i del arreglo
el par (j,p) donde p es el peso de la arista
@param a verice a conectar
@param b vertice a conectar
@param c peso de la arista que conecta a a y b
*)                
let conecta a b c g = let n = (Array.length g)-1 in
                      if (a < 0 || a > n || b < 0 || b > n)
                      	then raise(ErrorGrafica "Se ingreso un indice inexistente")
                        else
                        	let v1 = valida g.(a).vecinos b in
                        	let v2 = valida g.(b).vecinos a in
                        	if v1 && v2
                        	then
                        		((Hashtbl.add g.(a).vecinos b c);
                        		(Hashtbl.add g.(b).vecinos a c))
                        	else
                        		raise(ErrorGrafica "Vertices ya conectados")
                            
(**
regresa el peso de la arsita entre dos nodos a partir del indice de ambos
@param g grafica
@param u verice
@param v verice
*)
let getPeso g u v = Hashtbl.find g.(u).vecinos v


