# Fibonacci Heaps for OCaml
## Includes circular doubly linked lists

WIP

An implementation of priority queues using Fibonacci heaps in OCaml,
and a module for circular doubly-linked lists, since they are needed
for Fibonacci heaps.

In a Fibonacci heap, you keep an array containing pointers to a root
of each degree, and in the operation to extract the element with the
lowest priority, iterate on the list of roots for each degree to find
another root with the same degree, and make one the cild of the other,
increasing its degree by one, and repeat until all roots are of
different degrees.

It seemed easier to me to keep, for each degree, a list of all the
roots of this degree, and merge trees until all lists are empty or
contain a single element.

Because of this, some time complexities are not respected: the merge
operation should be *O*(1), but cannot be here, though I am not sure
how it can be in the original implementation, since the arrays of
pointers to roots of each degree need to be merged, which cannot be
done in constant time, I think.


Also I am not sure the extraction of the minimum respects the *O*(log
*n*) time complexity.

Overall I am not sure this implementation can really be called
"Fibonacci heaps", but it was a fun personnal project. Don't use these
if you need proper Fibonacci heaps in a project.
