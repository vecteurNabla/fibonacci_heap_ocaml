type 'a fibTree
(** The type for the nodes of a Fibonacci heap. *)

type 'a fibHeap
(** The type for Fibonacci heaps. *)
   
val make : unit -> 'a fibHeap
(** Creates a new, empty Fibonnaci heap. *)

val min : 'a fibHeap -> 'a * float
(** [min f] returns the key and priority of the minimum of [f]. *)
  
val add : 'a -> float -> 'a fibHeap -> unit
(** [add e p f] adds an element of key [e] with priority [p] to the heap [f]. *)
  
val merge : 'a fibHeap -> 'a fibHeap -> 'a fibHeap
(** [merge f f'] merges the two heaps. *)
  
val decrease_priority : 'a fibTree Cdlist.cdlist -> float -> 'a fibHeap
(** [decrease_priority n p f] sets the priority of node [n] from heap [f]
   to [p]. Make sure that [p] is smaller than the previous priority
   of node [n] and that [n] is actually part of [f]. Behaviour is
   undefined otherwise. *) 

val delete : 'a fibTree Cdlist.cdlist -> float -> unit
(** [delete n f] deletes node [n] from heap [f]. Behaviour is
   undefined if [n] is not in [f]. *)

val extract_min : 'a fibHeap -> int * float
(** [extract_min f] returns the element with the lowest priority,
   along with its priority, and deletes it from f. *)
                                     
