let (!.) = Cdlist.(!.)

type 'a fibTree = {
    key : 'a;
    mutable priority : float;
    mutable children : ('a fibTree) Cdlist.cdlist;
    mutable parent : ('a fibTree) Cdlist.cdlist
  }
;;
  
let ( </ ) n1 n2 =
  n1.priority < n2.priority
;;

let n_node k p = {
    key = k;
    priority = p;
    children = Cdlist.Nil;
    parent = Cdlist.Nil;
  }

type 'a fibHeap = {
    mutable min : 'a fibTree Cdlist.cdlist;
  }
;;

let make () = {
    min = Cdlist.Nil;
  }
;;

let min f =
  !.(f.min).da.key, !.(f.min).da.priority
;;

  
let add k p f =
  let root = Cdlist.from_list [n_node k p] in
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

let decrease_priority n p f = match !.n.da.parent with
  | Cdlist.Nil -> (
   !.n.da.priority <- p;
  )
  | _ -> (
    !.n.da.parent <- snd (Cdlist.pop n);
    add !.n.da.key p f
  )
;;


let extract_min f =
  let km = match f.min with
    | Nil -> failwith "Empty"
    | x -> (
      let e, k, c = (!.x).da.key, (!.x).da.priority,
                    (!.x).da.children in
      (Cdlist.iter (fun a -> !.a.da.parent <- Cdlist.Nil) c;
      if Cdlist.length x = 1 then
         f.min <- c
       else (
         let xri = (!.x).ri in
         let xle = (!.x).le in
         f.min <- xri;
         match c with
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
  let degs = Array.make (2 lsl n) Cdlist.Nil in
  let rec merge_nodes x y i =
    if (!.x).da </ (!.y).da then
      (let child, roots = Cdlist.pop y in
       let children = !.x.da.children in
       f.min <- x;
       !.x.da.children <- Cdlist.add child children;
       !.y.da.parent <- x;
       degs.(i) <- Cdlist.Nil;
       if degs.(i+1) <> Cdlist.Nil then
         merge_nodes x degs.(i+1) (i+1)
       else degs.(i+1) <- x;
      )
    else
      (let child, roots = Cdlist.pop x in
       let children = !.y.da.children in 
       f.min <- y;
       !.y.da.children <- Cdlist.add child children;
       !.x.da.parent <- y;
       degs.(i) <- Cdlist.Nil;
       if degs.(i+1) <> Cdlist.Nil then
         merge_nodes y degs.(i+1) (i+1)
       else degs.(i+1) <- y;
      )
  in
  let rec link a =
      let e, k, c = (!.a).da.key, (!.a).da.priority,
      (!.a).da.children in
      let d = Cdlist.length c in
      (if degs.(d) = Cdlist.Nil then
         degs.(d) <- a
       else
         merge_nodes a degs.(d) d
      );
      if not (!.a.ri == f.min) then
        link (!.a).ri
  in
  if f.min <> Cdlist.Nil then
    (
      link (f.min);
      Cdlist.iter (fun x ->
          if (!.x).da </ (!.(f.min)).da
          then f.min <- x) f.min
    );
  km
;;


let delete n f =
  decrease_priority n (-.infinity) f;
  ignore (extract_min f)
;;
