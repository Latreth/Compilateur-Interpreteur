PROJ2 - Rendu 2
GUEPIN Florent & KLINGELHOEFER Felix
Niveau Débutant

lexer.mll : Rien de particulier à signaler, c'est un lexer.
parser.mly : Traitement partiel du typage, avec les expressions numériques ( sauf quelques cas d'applications de fonctions avec trop peu d'arguments ) dans expr_arith, les expressions booléennes dans cond et les expressions générales de type expr.
main.ml : Traitement des différents cas en fonction des arguments passés lors de l'exécution ( rendu débutant donc, d'après ce que nous avons compris seulement le -debug ), avec récupération des erreurs de parsing. Il y a également l'affichage pour le résultat de l'interpréteur.
Expr.ml : Définition des types, et traitement de l'affichage pour le -debug.
Interpret.ml : Interpréteur
Fichier_test.txt : Exemples et résultats renvoyés par l'interpréteur.


A propos de l'interpréteur : 

On est partis dans un objectif de lecture facile du code effectué, ce qui explique notre choix de séparer le plus possible les types arithmétique, les conditions qui renverront des booléens et le type expr. 
Ceci explique aussi la présence de trois fonctions d'interprétation, au lieu d'une grosse. 
De plus, dans notre main se trouve une fonction d'affichage plutôt iposante. Ceci s'explique par notre volonté de reproduire au mieux l'affichage de fonction de caml.

Afin de faire marcher notre rendu : 

Il faut compiler avec la commande make dans l'invite de commande, puis on peut lancer le fichier fouine ainsi créé.
Pour lancer ce dernier, on lance : ./fouine (ceci pour ensuite rentrer un programme de notre choix)
Ou ./fouine -debug. Cette dernière option est présente afin de renvoyer le programme compilé dans le panneau de commande, afin d'eventuellement pouvoir lire les erreurs de parsing de notre programme.

Quelque erreurs :

1. Notre programme, lors du renvoi de la fonction en affichage (donc après interprétation), ne recompile pas un programme en affichage qui en serais pas déjà présent dans l'environnement. 
C'est à dire : 
si on rentre : fun y z -> let a = 8 in y*(z-a);; Alors la sortie sera : y -> (fun z -> let a = 8 in (y*(z-a)) = <fun>
Donc, on ne recompile pas la partie de déclaration du a. 

2. Le If/Then est traité, mais pour des soucis de typage, nous avons du renvoyer un 0 si la condition est fausse. En effet, caml oblige le type unit a la condition dans le then si on a uniquement un If/Then, or notre programme renvois forcément un type value ( à retransmettre ensuite ). Nous ne pouvions donc pas laisser notre Then au type value. Nous avons donc décider de mettre un rendu Int(0) si la condition était fausse. ( Nous n'arrivions pas à forcer le type autrement ) Nous aurions pu néanmoins fonctionner avec des références, mais cela ne nous a pas sembler etre dans l'esprit du sujet. 

Nous avons listé dans notre liste différents exemples usant toutes les fonctions disponible. Nous avons associé le retour machine effectué. Quelque erreurs volontaires et signalés sont dans ce fichier, pour vérifier que divers fausses écritures ne sont pas acceptés. 


Note importante :

Dans le sujet, il est écrit qu'un programme peut renvoyer un entier, une fonction et que les conditions booléennes n'existent que dans les if then else. Au risque de nous tromper, c'est ce que nous avons implémenter dans notre programme, c'est a dire que nos fonctions prennent en arguments des entiers, et renvoyent soit des fonctions, soit des entiers, les conditions booléennes étants finalement réservés aux if then else. En particulier, nos programmes ne prennent pas en entrée des programmes. 

Attention dans l'utilisation du -debug, sur erreur de syntaxe volontaire, il est possible qu'il ne reconnaisse pas le 1er ";;", en mettre un deuxième résout le problème, et renverra la bonne solution.

-------------------------------------------------------------------------------- Ajout suite au Rendu 3 -----------------------------------------------------------------------------------------------------

Tout d'abord, parlons du cas des fonctions récursives. Celles ci sont traités dans ce rendu, et ont été testés. Nous sommes partis en particulier des trois fonctions emblématiques récursives, par ordre de difficultés croissantes : La fonction Factorielle, la fonction de fibonnacci récursive, puis la fonction d'ackerman. Il nous a ensuite semblé être une bonne idée de tester les limites de notre programme, en testant notamment quelque valeurs limite a la fonction d'ackerman. On donne ici pour limite un temps d'éxecution raisonnable, de l'ordre de tout au plus 30 secs. Le détails des résultats est donné dans le fichier Fichier_test, test #20.

Il est a noter que dans l'écriture des fonctions récursives, on peut s'apercevoir d'une autre limite de nore programme, du au choix de typage précédemment expliqué dans le If Then simple. En effet, dans l'écriture d'un programme tel que fibonacci recursive, on ne peut pas utiliser des conditions simple tel que le If Then -> le programme va compiler en utilisant un Int(0) dans l'interpréteur, ce qui va rendre impossible la lectuer de la suite. Et va donc renvoyer une erreur. Cette erreur est facilement contournable en utilisant la condition If Then Else classique. Cela va leurrer l'interpréteur et nous pourrons donc continuer tranquillement l'implémentation de notre fonction récursive. (cf Fichier_Test #18)

De plus, dans l'implémentation de fonction récursive arrive l'utilisation d'opérateur dans l'expression récursive (tel que fact N-1) notre programme, comme en caml, ne va pas alors réussir a parser correctement (cf Fichier_Test #17) d'ou l'importance alors de l'utilisation de parenthèse adéquate. (fact(N-1))

Nous avons supprimé tout les warnings du a la compilation évoqués dans le Rendu 2 via un matching complet, renvoyant des messages d'erreurs le cas échéant. Nous avons de plus regarder l'erreur indiqué dans le mail de retour (a savoir : dans la fonction let f x y =x*y in f (prInt 3) (2 + prInt 2) cela renvoyait 3  2 12 au lieu de 2 3 12 dans l'ordre caml.) 
Après avoir changer un ordre d'appel dans l'interpréteur, le retour sur cette fonction est bien l'ordre demandé par Caml, a savoir : 2 3 12.

Nous avons aussi essayé de gérer aux mieux la disctinctions des erreurs, en modifiant notamment la position du try _ with afin que celui ci n'affiche snitaxe invalide qu'en cas de problème dans le parsage. 

Pour la partie de la machine a pile : Comme demandé dans le sujet, nous ne traitons que le cas d'opréations arithmétiques. Pour ce faire la division du travail a été la suivante : une partie sur la compilation, une autre sur sa lecture via la machine a pile. Ainsi, les deux fichiers suivants on été rajoutés par rapport au rendu 2 : Machine_a_pile.ml et Compile.ml

Dans le main, il a alors été éffectué divers modifications liés : 1 a un renommage plus clairs des fonctions utilisés 2 un ajout de diverses fonctions, de tels manières a faire marcher l'option -machine et l'option -interm. 
Utilisation de l'option -interm : mettre en argument n°2 un fichier du type : output.txt il sera alors écrit a l'intérieur le résultat de la compilation vers la machine a pile. L'option -machine n'attends aucun argument suplémentaire. 

Nous avons ensuite ajouté une série de 10 exemples avec l'utilisation de ces options au fichier Fichier_Test (les test_cases n°20 a 30) 
Suite a ces exemples, afin de traiter les nombres négatifs seuls (type -35) dans notre programme, il faut rentrer : 0-35 en effet, le nombre de départ entré (-35) ne parse pas correctement, puisqu'il s'agit d'un opérateur, le "-" seul pose problème.
Remarque : Les opérations a conditions, du type : If 3=8 then 1 else 0 ne marche pas même sur le rendu machine, en effet, l'option -machine ne dois traiter qu'exclusivement les opérations arithmétiques.


Pour finir, les deux derniers test du fichier Fichier_Test (#31 et #32) sont la pour montrer qu'on ne peut pas utiliser n'importe comment l'éxécutable ./fouine. 

-----------------------------------------------------------------------------------Ajout suite au Rendu 4 ---------------------------------------------------------------------------------------------------

Dans ce rendu 4, nous avons implementé les exceptions et les réfèrences. La syntaxe de lecture d'une exception dans notre proramme (unique) est de la forme : try expr(avec ou sans raise (E expr)) with E x -> expr. Pour les références, nous implémentons de la facon suivante : let x = ref expr (in) puis on modifie une valeur grace a : x:= expr et enfin on accede a la valeur grace a : !x il faut donc trois nouveaux types dans le type expr précedemment implémenté pour les références, et une pour les expections, ainsi qu'un ajout d'un type Exception, et d'un type reference dans le type de retour : value. 

Suite à de multiples test décrit dans le fichier Fichier_Test, nous avons conclu que la partie demandé était concluante. Vous trouverez dans ce fichier les exemples que nous avons traités en guise de debug de notre programme. La remarque envoyé par mail a bien été traité, a savoir que la phrase de commande : try raise (E 17) +2 with E x -> x+1 retourne bien 18 et non 19 ou 20 (on ne fait que le calcul du with a partir de la valeur de l'exception. 

A partir de la base du mail de retour du rendu 3, nous avons corrigé toutes les erreurs signalés. C'est a dire, que des phrases du type : let x = 32 in (x*(let x = 53 in x)) retourne bien 32*53 dorenavant, ou bien : (let x = 32 in 52*x). Enfin, nous pouvons lire les données a partir d'un fichier existant (renvois une erreur si non existance) et écrire le retour dans un fichier de destination. Chose non implémenté dans les précedents rendus. 

Remarque :
Dans l'implémentation globale, on ne peut pas lire : raise(E 17) + !r tandis que raise(E 17) + (!r) est possible. (voir fichiertest #37-38) en effet, les parenthèses sont nécessaires pour veiller au bon parsage de notre ligne de commande. 
De plus, on ne peut effectuer qu'une ligne de commande dans notre programme (niveau débutant) puisque la séparation de commandes, illustré par le ";" n'est pas implémenté. Ainsi, l'nterêt des références est très limité, puisque leur changement de valeur prend une ligne de commande. Ainsi, dans un soucis de clarté du code, ainsi que pour faciliter la lecture du résultat, nous avons pris la liberté d'implementé l'affectation d'une manière un peu plus libre : on affecte tout d'abord a la référence comme demandé, puis on afiche cette référence (ceci ne devrait pas etre présent) nous sommes conscient de cette différence, mais sans cela, aucun test sur l'affectation de valeur à une référence n'était possible de par la raison évoqué précedemment. 
Il s'agit d'une ligne de code, à savoir supprimer le ";!v" dans l'interperteur ligne 142 (Interpret.ml). 
Nous somme conscient que l'implémentage des references n'est pas optimal, puisque l'accès se fait imperativement à l'aide des parenthèses, mais cela ne concernant que cette partie des réfèrences, nous l'avons laissé tel quel. 

Pour utiliser l'option de fichier : ./fouine fichier.ml et on est obligé de mettre un fichier output.txt dans l'appel d'interm (qui existe).
Nous avons aussi joins a ce dossier (dans la meme archive) les fichiers beamer et pdf qu'il était demandé en conclusion de ce rendu.


