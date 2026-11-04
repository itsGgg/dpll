# Mini-projet 1 : solveur DPLL récursif

## Objectif du mini-projet

Le but du mini-projet est d'implémenter un solveur DPLL récursif en
OCaml.

## Compiler son projet

Le projet est fourni avec les fichiers de configuration permettant de le compiler avec le système `dune`.
`dune` et les dépendances nécessaires ont été installées sur les machines de l'UFR d'Informatique.
(Sous un système de type debian/ubuntu/…, cela peut nécessiter l'installation de paquets via la commande `sudo apt-get install ocaml-dune libppx-inline-test-ocaml-dev libppx-inline-test-ocaml libppx-assert-ocaml libppx-assert-ocaml-dev libppx-hash-ocaml libppx-hash-ocaml-dev libppx-enumerate-ocaml libppx-enumerate-ocaml-dev libjane-street-headers-ocaml libjane-street-headers-ocaml-dev`)

Pour compiler votre programme, placez vous dans le répertoire `dpll_solver` et tapez:

```
dune build
```

Pour exécuter votre programme, il faut utiliser la commande:

```
dune exec dpll_solver ../examples/SAT/sudoku-4x4.cnf
```

ce qui devrait répondre

```
SAT
-111 -112 113 -114 -121 -122 -123 124 -131 132 -133 -134 141 -142 -143 -144 -211 212 -213 -214 221 -222 -223 -224 -231 -232 -233 234 -241 -242 243 -244 311 -312 -313 -314 -321 322 -323 -324 -331 -332 333 -334 -341 -342 -343 344 -411 -412 -413 414 -421 -422 423 -424 431 -432 -433 -434 -441 442 -443 -444 0
```

D'autres exemples au format DIMACS sont disponibles dans le répertoire
`examples` (et `exemple_3_12`, `exemple_7_2`, `exemple_7_4`, `exemple_7_8`,
`systeme`, `coloriage` dans `test`).
