

{- QUESTION 1
   Écrire la fonction 'simplifie' qui permet de simplifier une fraction
   représentée ici par un couple (numérateur, dénominateur).
   Vous pouvez utiliser la fonction 'gcd' du Prelude. 
   Essayer de rendre la fonction le moins dépendant possible de 
   la représentation interne de la franction, c'est-à-dire le 
   moins dépendant possible du fait qu'une fraction est actuellement
   représentée par un couple d'Integer.
   -}

cree_fraction :: Integer -> Integer -> (Integer, Integer)
cree_fraction n 0 = (0,1) 
cree_fraction n d = simplifie (n,d)

numerateur :: (Integer, Integer) -> Integer 
numerateur (n,_) = n

denominateur :: (Integer, Integer) -> Integer 
denominateur (_,d) = d 

simplifie :: (Integer, Integer) -> (Integer, Integer)
simplifie fraction = (div(numerateur fraction)mdc,div(denominateur fraction)mdc)
                where mdc=gcd(numerateur fraction)(denominateur fraction)


{- QUESTION 2
   Écrire une fonction qui prend une liste de listes d'éléments 
   et qui la transforme en une liste aplanie des éléments.
   Ceci revient à "ajouter" les listes les unes aux autres à
   l'aide de l'opérateur (++)
   Par exemple, la liste
       [[1,2,3], [4,5], [6,7,8,9]] 
   doit devenir la liste
       [1,2,3,4,5,6,7,8,9]
   -}

aplanir :: [[a]] -> [a]
aplanir [] = [] 
aplanir  (l:ls) = l ++ aplanir ls
{- QUESTION 3
   Écrire la fonction 'substitue' qui a le comportement suivant :
   'substitue x y liste' échange les 'x' par des 'y' dans 'liste'
   -}

substitue :: (Eq a) => a -> a -> [a] -> [a] 
substitue x y[] = []
substitue x y (h:t) = if h== x then y : substitue x y t 
                       else h : substitue x y t  



{- QUESTION 4 
   Définir une fonction 'hanoi' qui produit une chaîne de caractère
   donnant la solution aux problèmes des tours de Hanoï.
   En particulier la fonction prendra en paramètre 4 arguments.
   Les trois derniers seront des Char représentant, respectivement,
   les tours 'origine', 'final' et 'auxilliaire'. Le premier  
   argument correspondra au nombre de disque à déplacer de la tour
   'origine' vers la tour 'finale' en utilisant la tour 'auxilliaire'.
   La solution doit être par des phrases de la forme "tour_1 vers tour_2"
   indiquant un déplacement du plus haut anneau de la tour 1 vers la tour 2.
   Ainsi par exemple l'appel 'hanoi 2 x y z' doit retourner la String 
   s'affichant à l'écran sous la forme suivante.
       x vers z
       x vers y
       z vers y
   Comme dans la plupart des langages de programmation, le saut de ligne
   est obtenu par le caractère \n dans une String.

   De l'information supplémentaire et des algorithmes solutions sont 
   disponibles sur wikipedia : http://fr.wikipedia.org/wiki/Tours_de_Hanoï
   -}





hanoi :: Integer -> Char -> Char -> Char -> String
hanoi n start end aux
   | n == 1 = moveRing
   | n /= 0 = (hanoi (n-1) start aux end) ++ "\n" ++ moveRing ++ "\n" ++ (hanoi (n-1) aux end start)
   where moveRing = ([start] ++ " vers " ++ [end])