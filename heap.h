// ======================================================================
// heap
// ======================================================================
// for now the heap is a single array of cells.  later we will
// implement garbage collection by dynamically allocating heaps and
// copying live values into them as-needed, recycling old heaps as we
// go

#define HEAP_SIZE 262144 // number of available cells in a heap

struct cell heap[HEAP_SIZE];
struct cell* free_pointer;
