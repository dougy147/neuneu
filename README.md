Construct basic neural networks in Common Lisp.

```console
$ cat ./examples/xor.txt
0 0 | 0
0 1 | 1
1 0 | 1
1 1 | 0
```

```lisp
(load-training-data-from-file "./examples/xor.txt")
(setq *network-architecture* '(2 1)) ; list of neurons by layer { 2 x 1 }
(construct-network)

(learn 10000) ; e.g. 10000 iterations
(check-results)
```

```console
$ ./main.fasl
...
------------------------------------
  input          : #(0 0)
  expected output: #(0)
  network output : #(0.10375877)
------------------------------------
  input          : #(0 1)
  expected output: #(1)
  network output : #(0.8785281)
------------------------------------
  input          : #(1 0)
  expected output: #(1)
  network output : #(0.8776233)
------------------------------------
  input          : #(1 1)
  expected output: #(0)
  network output : #(0.12952015)
------------------------------------
```

No backprop for now.
