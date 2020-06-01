type 'a cdlist = Nil | Cons of 'a cell
and 'a cell = {mutable le: 'a cdlist; da: 'a; mutable ri: 'a cdlist}
(** A type for doubly-linked lists in general, though the functions
   below implement circular doubly-linked lists*)

            
val (!.) : 'a cdlist -> 'a cell
(** A useful shortcut: [!.l] returns x if [l = Cons(x)], raises a
   Failure error if [l = Nil]. *)

val from_list : 'a list -> 'a cdlist
(** [from_list l] returns a circular doubly linked list containing
   the same elements as [l]. The result is a pointer towards a cell
   that contains the first element of [l], its right neighbour
   contains the second element of [l], and so on. *)

val length : 'a cdlist -> int
(** Returns the length of the list. *)
  
val iter : ('a cdlist -> unit) -> 'a cdlist -> unit
(** Similar to List.iter:
   [iter f l] applies f to each element of the list l, starting from
   the one f is pointing to, going right. *)
val add : 'a -> 'a cdlist -> 'a cdlist
(** [add x l ] adds a new cell containing x to the list l: it creates
   a new list if [l = Nil], adds it in between l and its left
   neighbour. *)
  
val pop : 'a cdlist -> ('a * 'a cdlist)
(** [pop l] pops the first cell from the list (the one l points to),
   and returns a pair of the content of the cell, and a pointer
   towards what was the right neighbour of l.
   Warning: l now points to a cell that is not part of the list,
   though the cell's right and left neighbour still are. *)
