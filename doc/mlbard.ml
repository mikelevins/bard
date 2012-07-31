(* an experiment in using an ML-like syntax for bard *)

define Puzzles = {}

let idnum = 0
in
  define function componentID = 
    set! idnum = (idnum+1);
    idnum;

define function addPuzzle puzzle =
  let title = get puzzle 'title
  in
    set! Puzzles = put Puzzles title puzzle;
    puzzle;

define function readPuzzle text =
    eval (readText text);

define function readPuzzleFile path =
    readPuzzle (readFile path);





