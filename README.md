# **Démineur**
Projet en elm du jeu démineur dans le cadre du cours `Programme fonctionnel Avance`  
**Groupe : Ayoub Bhija, Achraf Hanini, Alexandre Em**  
Le principe du démineur est de trouver toute les mines dans une grille de `8`x`8` (modifiable dans le code du fichier `Main.elm` et `main.css`), sans en déclencher une.  

Pour revele une case il faut faire un `click gauche` sur une case et pour marquer une case que l'on pense etre une mine, il faut alors mettre un `flag` avec le `click droit` de la souris 

Le projet à été divisé en 3 parties, donc 1 tache par personne:
- Initialisation de la grille
- Creation de la view (HTML/CSS)
- Implémentation de l'algorithm Flood fill

puis nous avons terminé le projet, par la gestion de l'evenement de **fin de jeu**, donc lorsque l'utilisateur clique sur une mine.

## **Installation du projet**
```sh
$ cd demineur
$ npm install
$ npm run start
```
Lancer les tests:
```sh
$ elm-test
```

## **Structure du code**
Le programme est separe en *3 fichiers*: `Mine.elm`, `main.css` et `Main.elm`.
- `Mine.elm`: qui contient le code de generation aleatoire des positions des mines dans la grille.
- `main.css`: qui contient le style de la page
- `Main.elm`: qui est separe en deux parties:
    - La partie logique, qui comprend l'initialisation de la grille, sa mise a jour avec l'algo `floodfill` dont chaque case contient une `Case` definit comme ci dessous: 
        ```haskell
        type Case
            = Hint ( Int, Int ) Int Bool Bool
            | Mine ( Int, Int ) Bool Bool
            | Empty ( Int, Int ) Bool Bool
        ```
        ou le tuple de `Int` est la position dans la grille, le 1er `Bool` est pour savoir si la case est `visible`, le 2e est pour savoir si le joueur a place un `flag`. Dans `Hint`, le `Int` est le nombre de `Mines` autour de cette case.  
        le model est donc defini comme ci dessous:
        ```haskell
        type alias Model =
            { grid : List Case, width : Int, height : Int, onGoing: Bool }
        ```
    - La partie View, qui est une `grid` de css, ou chaque case est un `Html Msg` obtenu par une transformation de la grille du model en appelant la fonction `List.map` ou on traite chaque case de `Case` pour afficher:
        - une case non revele
        - 🚩: si la case a ete marque, `n`
        - un numero qui correspond au nombre de mines a proximite
        - 💣: si la case comporte une mine
        - une case vide d'une couleur differente de la case non revele


## **Problemes rencontres**
- Recuperation, Mise a jour de la grille: il n'y a pas de fonctions dans le module `List` qui permet de recupere, de modifier un element dans la list. Nous avons alors cree un type qui contient le numero de la ligne et colonne, pour ensuite utilise `List.filter` afin de recupere un element de la grille et `List.map` afin de modifier un element dans la list. Cela implique la grille ne doit pas etre trop grande, ce qui causerait une erreur de nombre de recursion.
- Niveau syntax, lorsque l'on veut recupere le `head`/`tail` d'une liste, les fonctions renvoient un type `Maybe` et lorsqu'il y a deja plusieurs pattern matching, on s'est a plusieurs reprise perdu. Pour cela, on a du separe le code en plusieurs petites fonctions, afin que ce soit plus visible.