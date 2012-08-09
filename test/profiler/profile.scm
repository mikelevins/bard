
(load "/Users/mikel/Projects/bard/load.scm")
(load-bard)
(%init-bard)

(profile-start!)
(begin
  (%eval '(load "/Users/mikel/Projects/bard/test/schemas.bard") '())
  (%eval '(load "/Users/mikel/Projects/bard/test/Puzzle.bard") '())
  (%eval '(load "/Users/mikel/Projects/bard/test/arctic-disaster-1.bard") '()))
(profile-stop!)
(write-profile-report "bard-eval")
