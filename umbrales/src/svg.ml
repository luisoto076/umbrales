(**Modulo para generar las graficas del la solucion y el comportamiento de las evaluaciones*)
open Printf
open Grafica

(*nombre del archivo que depende de el origen y destino de la ruta a calcular*)
let file nombre t = nombre^t^".svg"


(*Esqueleto que regresa una cadena para construir una linea en svg, recive las cordenadas de 
  los puntos de inicio y fin de la linea y el color
 *)
let creaLinea oc x1 y1 x2 y2 color =
	let c = 13. in
	let k = 70. in
	let l = 125. in
	let xe1 = c *. (x1 +. l) in
	let ye1 = c *. (k -. y1) in
	let xe2 = c *. (x2 +. l) in
	let ye2 = c *. (k -. y2) in
	Printf.fprintf oc "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke=\"%s\" stroke-width=\"2\" />\n" xe1 ye1 xe2 ye2 color


let creaLineaf oc x1 y1 x2 y2 color =
	let c = 0.7 in
	let k = 100. in
	let xe1 = c *. x1 in
	let ye1 = k *. y1 in
	let xe2 = c *. x2 in
	let ye2 = k *. y2 in
	Printf.fprintf oc "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke=\"%s\" stroke-width=\"1\" />\n" xe1 ye1 xe2 ye2 color


(*Esqueleto que regresa una cadena para construir un circulo en svg, recive las cordenadas del
  centro y el color. El radio es fijo
 *)
let creaCirculo oc x y r color =
	let c = 13. in
	let k = 70. in
	let l = 125. in
	let xe = c *. (x +. l) in
	let ye = c *. (k -. y) in
	Printf.fprintf oc "<circle cx=\"%f\" cy=\"%f\" r=\"%d\" stroke=\"black\" stroke-width=\"2\" fill=\"%s\" />\n" xe ye r color

let creaCirculof oc x y r color =
	let c = 0.7 in
	let k = 100. in
	let xe = c *. x  in
	let ye = k *. y in
	Printf.fprintf oc "<circle cx=\"%f\" cy=\"%f\" r=\"%d\" stroke=\"black\" stroke-width=\"1\" fill=\"%s\" />\n" xe ye r color

(*Esqueleto que regresa una cadena para construir un componente de texto, recive las cordenadas del 
  centro del area de texto, el color del mismo y el contenido.
 *)
(*let creaTexto x y color txt = "<text x=\""^x^"\" y=\""^y^"\" fill=\""^color^"\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"middle\">"^txt^"</text>\n"*)

(*Regresa el encabezado del svg, recive el tamaño de una lado de la imagen, la imagen siempre
  sera cuadrada*)
let encabezado oc h w =
	Printf.fprintf oc  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<!DOCTYPE svg>\n<svg height=\"%d\" width=\"%d\">\n"  w h

(*función que contruye el contenido del svg a partir de la informacion de la grafica y la ruta*)                       
let svg oc g s =
	let n = Array.length s in
	encabezado oc 3500 1500;
	for i = 0 to n-2 do
		let u =  g.(s.(i)) in
		let v =  g.(s.(i+1)) in
		creaLinea oc u.longitud u.latitud v.longitud v.latitud "red"
	done;
	for i = 1 to 278 do
		let u =  g.(i) in
		creaCirculo oc u.longitud u.latitud 4 "blue" 
	done;
	fprintf oc "\n</svg>"
               
(*funcion que guarda en el archivo indicado el svg*)
let guarda g s t = 
  (* Write message to file *)
  let oc = open_out (file "funcion" (string_of_int t))  in    
  svg oc g s;  
  close_out oc
  
let gfuncion arr t =
	let oc = open_out (file "grafica" (string_of_int t))  in      
	let n = (Hashtbl.length arr)/5000 in
	let j = ref 1 in
	encabezado oc 4000 1500;
	for i = 1 to 4999 do
		creaCirculof oc (float_of_int !j) (Hashtbl.find arr (n*i)) 4 "blue";
		creaLineaf oc (float_of_int !j) (Hashtbl.find arr (n*i)) (float_of_int !j) (Hashtbl.find arr (n*(i+1))) "black";
		j:=!j+1
	done;
	fprintf oc "\n</svg>";
	close_out oc
