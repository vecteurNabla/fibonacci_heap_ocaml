let (!.) = Cdlist.(!.)

type 'a fibTree = Node of 'a * ('a fibTree) Cdlist.cdlist ref
;;
  
let ( </ ) = function
      | Node((_, a), _) -> function
                         | Node((_, b), _) -> a < b
;;

type fibHeap = {
    mutable min : (int * float) fibTree Cdlist.cdlist;
    degs : (int * float) fibTree Cdlist.cdlist list array;
  }
;;

let make () = {
    min = Nil;
    degs = Array.make 612 []
  }
;;

let add e k f =
  let root = Cdlist.from_list [Node((e, k), ref Cdlist.Nil)] in
  match f.min with
  | Nil -> (
    f.min <- root;
    f.degs.(0) <- root::f.degs.(0)
  )
  | x -> (
    let rimin = (!.x).ri in
    (!.x).ri <- root;
    (!.root).le <- x;
    (!.root).ri <- rimin;
    (!.rimin).le <- root;
    if (!.root).da </ (!.(f.min)).da then f.min <- root;
    f.degs.(0) <- root::f.degs.(0)
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
      degs = Array.map2 (fun a b -> a@b) f.degs f'.degs 
    }
  )     
;;


let extract_min f =
  let km = match f.min with
    | Nil -> failwith "Empty"
    | x -> (
      let Node ((e, k), c) = (!.x).da in
      let n = Cdlist.length !c in
      f.degs.(n) <- List.filter (fun d -> not (d == x)) f.degs.(n);
      Cdlist.iter (fun ch -> let Node(_, chi) = (!.ch).da in
                             let d = Cdlist.length !chi in
                             f.degs.(d) <- ch::f.degs.(d)
        ) !c;
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
  while Array.exists (fun l ->
            List.length l > 1
          ) f.degs
  do
    Array.iteri (fun i l ->
        match l with
        | x::y::r -> (
          if (!.x).da </ (!.y).da then
            (let child, roots = Cdlist.pop y in
             let Node (_, children) = (!.x).da in
             f.min <- roots;
             children :=  Cdlist.add child !children;
             (f.degs).(i+1) <- x::(f.degs).(i+1);
             (f.degs).(i) <- r)
          else
            (let child, roots = Cdlist.pop x in
             let Node(_, children) = (!.y).da in
             f.min <- roots;
             children := Cdlist.add child !children;
             (f.degs).(i+1) <- y::(f.degs).(i+1);
             (f.degs).(i) <- r)
        )
        | _ -> ()
      ) f.degs
  done;
  Cdlist.iter (fun x ->
      if (!.x).da </ (!.(f.min)).da
      then f.min <- x) f.min;
  km
;;

