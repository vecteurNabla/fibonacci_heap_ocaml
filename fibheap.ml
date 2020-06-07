let (!.) = Cdlist.(!.)

type 'a fibTree = Node of 'a * ('a fibTree) Cdlist.cdlist ref
;;
  
let ( </ ) = function
      | Node((_, a), _) -> function
                         | Node((_, b), _) -> a < b
;;

type fibHeap = {
    mutable min : (int * float) fibTree Cdlist.cdlist;
  }
;;

let make () = {
    min = Nil;
  }
;;

let add e k f =
  let root = Cdlist.from_list [Node((e, k), ref Cdlist.Nil)] in
  match f.min with
  | Nil -> (
    f.min <- root;
  )
  | x -> (
    let rimin = (!.x).ri in
    (!.x).ri <- root;
    (!.root).le <- x;
    (!.root).ri <- rimin;
    (!.rimin).le <- root;
    if (!.root).da </ (!.(f.min)).da then f.min <- root;
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
    }
  )     
;;


let extract_min f =
  let km = match f.min with
    | Nil -> failwith "Empty"
    | x -> (
      let Node ((e, k), c) = (!.x).da in
      (if Cdlist.length x = 1 then
         f.min <- !c
       else (
         let xri = (!.x).ri in
         let xle = (!.x).le in
         f.min <- xri;
         match !c with
         | Nil -> (
           (!.xri).le <- xle;
           (!.xle).ri <- xri;
         )
         | l  -> (
           let lri = (!.l).ri in
           (!.xle).ri <- lri;
           (!.lri).le <- xle;
           (!.l).ri <- xri;
           (!.xri).le <- l;
         )
       )
      );
      (e, k)
    )
  in
  let n = Cdlist.length f.min in
  let degs = Array.make (1 lsl n) Cdlist.Nil in
  let rec merge_nodes x y i =
    if (!.x).da </ (!.y).da then
      (let child, roots = Cdlist.pop y in
       let Node (_, children) = (!.x).da in
       f.min <- x;
       children :=  Cdlist.add child !children;
       degs.(i) <- Cdlist.Nil;
       if degs.(i+1) <> Cdlist.Nil then
         merge_nodes x degs.(i+1) (i+1)
       else degs.(i+1) <- x;
      )
    else
      (let child, roots = Cdlist.pop x in
       let Node(_, children) = (!.y).da in
       f.min <- y;
       children := Cdlist.add child !children;
       degs.(i) <- Cdlist.Nil;
       if degs.(i+1) <> Cdlist.Nil then
         merge_nodes y degs.(i+1) (i+1)
       else degs.(i+1) <- y;
      )
  in
  let rec link a =
      let Node((e, k), children) = (!.a).da in
      let d = Cdlist.length !children in
      (if degs.(d) = Nil then
         degs.(d) <- a
       else
         merge_nodes a degs.(d) d
      );
      if not (a == f.min) then
        link (!.a).ri
  in
  link !.(f.min).ri;
  Cdlist.iter (fun x ->
      if (!.x).da </ (!.(f.min)).da
      then f.min <- x) f.min; 
  km
;;

