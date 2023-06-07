# Compte rendu
## interp/interp.ml
Concernant ce premier interpréteur OCaml je n'ai pas vraiment fait de choix, l'implémentation était assez linéaire et je n'ai pas vraiment rencontré de problèms. J'ai juste du faire attention à l'ordre dévaluation du code pour les tables et les arguments.

## interp/mod.rs et interp/env.rs
Pas vraiment de choix non plus. L'ajout des lifetime était assez naturel pour env.rs. Et mod.rs était quasiment du copier coller de interp.ml.

## interp-cps/interp.ml
Le passage en intepréteur cps s'est fait sans trop de soucis, j'ai même réussi à convertir mes fonctions iter et map utilisée pour les arguments. 

Par contre les coroutines m'on posées pas mal de fil à retordre. Notamment la continuation pour Coroutine.create.

## Extension (extension.lua)
J'ai choisi d'implémenter un générateur de nombres premiers et un générateur de nombres pseudo-aléatoires. Pour cela j'ai dû rajouter le modulo dans le langage, ce qui s'est averré être facile car il suffisait de modifier simplify.ml, l'ast et de rajouter un cas dans interp.ml.