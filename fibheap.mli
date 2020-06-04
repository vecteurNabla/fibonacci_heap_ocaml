type fibHeap
(** The type for Fibonacci heaps. *)
   
val make : unit -> fibHeap
(** Creates a new, empty Fibonnaci heap. *)
  
val add : int -> float -> fibHeap -> unit
(** [add e p f] adds an element of key [e] with priority [p] to the heap [f]. *)
  
val merge : fibHeap -> fibHeap -> fibHeap
(** [merge f f'] merges the two heaps. *)
  
val extract_min : fibHeap -> int * float
(** [extract_min f] returns the element with the lowest priority,
 ** along with its priority, and deletes it from f. *)
                                     
