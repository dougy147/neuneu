(load "src/matrices.lisp")
(load "src/network.lisp")
(load "src/lexer.lisp")

(setf *random-state* (make-random-state t))

(defun main ()
  (setq *eps*  1e-1)
  (setq *rate* 1e-1)
  (setq *stop* 1e-2)

  (load-training-data-from-file "./examples/xor.txt")
  (setq *network-architecture* '(2 1)) ; list of neurons by layer { 2 x 1 }

  (construct-network)
  (learn 10000)
  (check-results))

(main)
