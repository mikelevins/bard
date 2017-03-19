#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "bard.h"
#include "cell.h"

// ======================================================================
// ABOUT
// ======================================================================
// compile with:
// gcc -O2 -o bard bard.c

  
// ======================================================================
// Main Program
// ======================================================================

int main (int argc, char*argv[]) {

  // timing
  clock_t start_time, end_time, elapsed;
  double diff;

  // ----------------------------------------------------------------------
  // BEGIN VM
  // ----------------------------------------------------------------------

  start_time = clock();

  struct cell heap[HEAP_SIZE];
  struct cell* free_pointer = 0;
  
  printf("\n\n========================================================================");
  printf("\nBard 0.4 VM");
  printf("\n  cell size = %ld bytes",(sizeof (struct cell)));
  printf("\n  heap size = %ld bytes",(sizeof heap));
  printf("\n  free_pointer = %p", free_pointer);
  printf("\n========================================================================\n");

  printf("\nStarting execution...\n");
    
  // ----------------------------------------------------------------------
  // END VM
  // ----------------------------------------------------------------------
    
  // shut down, report statistics
  end_time = clock();
  elapsed = end_time - start_time;
  diff = (double)elapsed;

  printf("\nRan to completion in %ld microseconds\n", (elapsed));
  printf(" (%f seconds)\n", (diff/CLOCKS_PER_SEC));
  printf("\n========================================================================\n\n");
}
