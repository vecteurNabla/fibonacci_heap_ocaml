type 'a dList = Nil | Cons of 'a cell
and 'a cell = {mutable le: 'a dList; da: 'a; mutable ri: 'a dList }
;;

let ( !. ) = function
  | Nil -> failwith "Nil"
  | Cons(x) -> x

             
let cdlist_from_list l =
  let rec aux li = match li with
    | [] -> Nil
    | x::r -> Cons({le = Nil; da = x; ri = aux r})
  in
  let dl = aux l in
  let tmp = ref Nil in
  let rec aux2 dli p = match dli with
    | Nil -> (tmp := p;)
    | x -> (
      (!.x).le <- p;
      aux2 (!.x).ri x;
    )
  in
  aux2 dl Nil;
  try
    (!.dl).le <- !tmp;
    (!.(!tmp)).ri <- dl;
    dl
  with _ -> Nil
;;



type 'a fibTree = Node of 'a * ('a fibTree) dList
;;
  
let ( </ ) = function
      | Node((_, a), _) -> function
                         | Node((_, b), _) -> a < b
;;

  

type fibHeap = {
    mutable min : (int * float) fibTree dList;
    degs : (int * float) fibTree dList array;
  }
;;

let new_fib_heap () = {
    min = Nil;
    degs = Array.make 1973 Nil
  }
;;

let add_key e k f =
  let root = cdlist_from_list [Node((e, k), Nil)] in
  match f.min with
  | Nil -> (
    f.min <- root;
    f.degs.(0) <- f.min
  )
  | x -> (
    let rimin = (!.x).ri in
    (!.x).ri <- root;
    (!.root).le <- x;
    (!.root).ri <- rimin;
    (!.rimin).le <- root;
    if (!.root).da </ (!.(f.min)).da then f.min <- root;
    if f.degs.(0) = Nil then f.degs.(0) <- root;
  )
;;


let merge f f' = match f.min, f'.min with
  | Nil, _ -> f'
  | _, Nil -> f
  | x, y -> (
    let xri = (!.x).ri in
    let yle = (!.y).le in
    (!.x).ri <- y;
    (!.y).le <- x;
    (!.xri).le <- yle;
    (!.yle).ri <- xri;
    {
      min = if (!.x).da </ (!.y).da then x else y;
      degs = Array.map2 (fun a b -> match a, b with
                                    | Nil, _  -> b
                                    | _, _ -> a ) f.degs f'.degs 
    }
  )     
;;


let extract_min f =
  let km = match f.min with
    | Nil -> failwith "Empty"
    | x -> let xri = (!.x).ri in
           let xle = (!.x).le in
           match (!.x).da with
           | Node((e, k), Nil) -> (
             (!.xri).le <- xle;
             (!.xle).ri <- xri;
             (e, k)
           )
           | Node((e, k), l) -> (
             let lri = (!.l).ri in
             (!.xle).ri <- lri;
             (!.lri).le <- xle;
             (!.l).ri <- xri;
             (!.xri).le <- l;
             (e, k)
           )
  in
  
