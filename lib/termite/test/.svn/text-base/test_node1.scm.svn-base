(node-init node1)
;;(debug (current-node))
(on node2 (lambda () (write 'success!) (newline)))

(define pid 
  (spawn 
	(lambda () 
	  (migrate-task node2)
	  (info 'migration-ok!))))



