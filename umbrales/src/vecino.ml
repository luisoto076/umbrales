
let inicializa n = Random.init n

let vecino s = let n = (Array.length s)-1 in
               let a = Random.int n in
               let b = Random.int n  in
               let aux = s.(a)
               in
                  s.(a) <- s.(b);
                  s.(b) <- aux
                  
let f s = let total = 0  
          in
            for i = 0 to (Array.length s)-2 do
              total = Grafica.getEdge(s.(i)) 
            done
                   
