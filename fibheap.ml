(* type 'a dList = Nil | Cons of 'a cell
 * and 'a cell = {mutable le: 'a dList; da: 'a; mutable ri: 'a dList }
 * ;;
 * 
 * let ( !. ) = function
 *   | Nil -> failwith "Nil"
 *   | Cons(x) -> x
 * 
 *              
 * let cdlist_from_list l =
 *   let rec aux li = match li with
 *     | [] -> Nil
 *     | x::r -> Cons({le = Nil; da = x; ri = aux r})
 *   in
 *   let dl = aux l in
 *   let tmp = ref Nil in
 *   let rec aux2 dli p = match dli with
 *     | Nil -> (tmp := p;)
 *     | x -> (
 *       (!.x).le <- p;
 *       aux2 (!.x).ri x;
 *     )
 *   in
 *   aux2 dl Nil;
 *   try
 *     (!.dl).le <- !tmp;
 *     (!.(!tmp)).ri <- dl;
 *     dl
 *   with _ -> Nil
 * ;;
 * 
 * let cdlist_length l = match l with
 *   | Nil -> 0
 *   | _ -> 
 *      let rec aux i r =
 *        if r == l then i else
 *          aux (i+1) (!.r).ri
 *      in aux 1 (!.l).ri
 * ;;
 * 
 * let cdlist_iter f l = if l = Nil then () else
 *   let rec aux r =
 *     f r;
 *     if not (r == (!.l).le) then
 *       aux (!.r).ri
 *   in aux l
 * ;;
 * 
 * let cdlist_add x l = match l with
 *   | Nil -> cdlist_from_list [x]
 *   | h -> (
 *     let ret = Cons({le = (!.h).le;
 *                     da = x;
 *                     ri = h;
 *                 }) in
 *     (!.((!.h).le)).ri <- ret;
 *     (!.h).le <- ret;
 *     ret
 *   )
 * ;;
 * 
 * let cdlist_pop l = match l with
 *   | Nil -> failwith "list vide"
 *   | Cons(x) -> (
 *     let new_l = x.ri in
 *     (!.new_l).le <- x.le;
 *     (!.(x.le)).ri <- new_l;
 *     (x.da, new_l)
 *   )
 * ;;     *)

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
    degs = Array.make 1973 []
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
    | x -> let xri = (!.x).ri in
           let xle = (!.x).le in
           let Node ((e, k), c) = (!.x).da in
           match !c with
           | Nil -> (
             (!.xri).le <- xle;
             (!.xle).ri <- xri;
             (e, k)
           )
           | l  -> (
             let lri = (!.l).ri in
             (!.xle).ri <- lri;
             (!.lri).le <- xle;
             (!.l).ri <- xri;
             (!.xri).le <- l;
             (e, k)
           )
  in (* pour l'instant, f.min ne pointe vers rien (de correct) !!!! *)
  let pnt = ref Cdlist.Nil in (* pointer that will point to the remaining
                               * roots at the end of the loop *)
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
             pnt := roots;
             children :=  Cdlist.add child !children;
             (f.degs).(i+1) <- x::(f.degs).(i+1);
             (f.degs).(i) <- r)
          else
            (let child, roots = Cdlist.pop x in
             let Node(_, children) = (!.y).da in
             pnt := roots;
             children := Cdlist.add child !children;
             (f.degs).(i+1) <- y::(f.degs).(i+1);
             (f.degs).(i) <- r)
        )
        | _ -> ()
      ) f.degs
  done;
  f.min <- !pnt;
  Cdlist.iter (fun x ->
      if (!.x).da </ (!.(f.min)).da
      then f.min <- x) !pnt;
  km
;;

