Construct basic neural networks in Common Lisp.

```console
$ cat ./examples/xor.txt
0 0 | 0
0 1 | 1
1 0 | 1
1 1 | 0
```

```lisp
(load-training-data-from-txt-file "./examples/xor.txt")
(setq *network-architecture* '(2 1)) ; list of neurons by layer { 2 x 1 }
(construct-network)

(learn-backprop 2000) ; e.g. 2000 iterations
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

# Resources

- Machine Learning in C - [https://www.youtube.com/watch?v=PGSba51aRYU](https://www.youtube.com/watch?v=PGSba51aRYU)
- What is backpropagation really doing? - [https://www.youtube.com/watch?v=Ilg3gGewQ5U](https://www.youtube.com/watch?v=Ilg3gGewQ5U)
- Backpropagation - [https://en.wikipedia.org/wiki/Backpropagation](https://en.wikipedia.org/wiki/Backpropagation)
