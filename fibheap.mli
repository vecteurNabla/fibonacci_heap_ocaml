type 'a fibTree = Node of 'a * ('a fibTree) Cdlist.cdlist ref

val (</) : ('a * 'b) fibTree -> ('c * 'b) fibTree -> bool

type fibHeap = {
    mutable min : (int * float) fibTree Cdlist.cdlist;
    degs : (int * float) fibTree Cdlist.cdlist list array;
  }

val make : unit -> fibHeap

val add : int -> float -> fibHeap -> unit

val merge : fibHeap -> fibHeap -> fibHeap

val extract_min : fibHeap -> int * float
