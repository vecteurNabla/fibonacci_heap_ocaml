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
  
val extract_min : 'a fibHeap -> int * float
(** [extract_min f] returns the element with the lowest priority,
   along with its priority, and deletes it from f. *)
                                     
