# Priority Queue

This is an implementation of priority queue in common lisp. It is implemented by augmenting hashmap for constant acess of key for updation and binary heap. This includes
- Updating the value in queue on fly.
- Insert a value on fly.
- Top for acessing the top function
- Pop for removing the top element.
Any enteries in the queue must be given with a key function such that it gives the max priority min value(it is implemented as min heap).
