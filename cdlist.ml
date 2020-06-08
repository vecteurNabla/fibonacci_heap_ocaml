type 'a cdlist = Nil | Cons of 'a cell
and 'a cell = {mutable le: 'a cdlist; da: 'a; mutable ri: 'a cdlist}
;;


let ( !. ) = function
  | Nil -> failwith "Nil"
  | Cons(x) -> x
;;

let from_list l =
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


let length l = match l with
  | Nil -> 0
  | _ -> 
     let rec aux i r =
       if r == l then i else
         aux (i+1) (!.r).ri
     in aux 1 (!.l).ri
;;


let iter f l = if l = Nil then () else
  let rec aux r =
    f r;
    if not (r == (!.l).le) then
      aux (!.r).ri
  in aux l
;;


let add x l = match l with
  | Nil -> from_list [x]
  | h -> (
    let ret = Cons({le = (!.h).le;
                    da = x;
                    ri = h;
                }) in
    (!.((!.h).le)).ri <- ret;
    (!.h).le <- ret;
    ret
  )
;;


let pop l = match l with
  | Nil -> failwith "Empty"
  | Cons(x) -> (
    if length l = 1 then x.da, Nil else
    let new_l = x.ri in
    (!.new_l).le <- x.le;
    (!.(x.le)).ri <- new_l;
    (x.da, new_l)
  )
;;    
