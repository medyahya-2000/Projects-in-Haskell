{-| TP6 du cours IFT359 à remettre le 25 juillet 2014

    Remis par :
-}
module TP6 
    ( 
    -- * Question 1
    -- $Q1
      mapCPS
    , filterCPS

    -- * Question 2
    -- ** Énoncé 
    -- $Q2 
    
    -- ** Graphe et visite d'un graphe
    , Graph (..)
    , visite 
    -- ** Fonction à implémenter
    , visiteCPS

    -- * Question 3
    -- $Q3

    -- ** Les listes d'associations comme dictionnaire
    , Dictionnaire (..)
    , chercher
    , chercherTous
    , insérer 
    -- ** Fonctions à implémenter
    , chercherCPS
    , chercherTousCPS 
    , insérerCPS 

    -- * Question 4
    -- $Q4 

    -- ** Type de donnée représentant les calculs en CPS 
    , Continuation (..) 
    -- ** Exemples d'utilisation simple
    , produit 
    , carre 
    , produitCarre 
    -- ** Fonctions utiles
    , map'
    , filter'
    , reverse' 
    , transitions'
    -- ** Fonctions à implémenter 
    , chercher'
    , chercherTous'
    , insérer' 
    , visite'

    -- * Exemples et tests 
    , testsQ1
    , testsQ2
    , testsQ3
    , testsQ4 
    ) where 

import Data.Dictionnaire 
import Data.List (sort) 

{- $Q1
    implémenter une version CPS des opérateurs classiques 'map' et 'filter'
-}

mapCPS :: ([b] -> t) -> (a -> b) -> [a] -> t
mapCPS = undefined 

filterCPS :: ([a] -> t) -> (a -> Bool) -> [a] -> t
filterCPS = undefined 

{- $Q2
    Les structures de graphes sont très utilisées en informatique.
    Le type @Graph@ représente une implémentation simple d'un graphe
    en fournissant uniquement la fonction de transition. 
    La fonction 'visite' permet de calculer les états qui peuvent
    être atteints dans un graphe à partir d'un état initial

    > visite graphe s0

    calcule les sommets visités à partir du sommet @s0@ dans le graphe @graphe@.
    Vous devez implémenter une forme CPS de la fonction 'visite'. 
-}

data Graph a = G { transitions :: a -> [a] }

{- | @'visite' graphe s0@ calcule les sommets visités à partir du sommet 
     @s0@ dans le graphe @graphe@ 
-} 
visite :: Eq a => Graph a -> a -> [a] 
visite graphe s0 = visite' [s0] []
    where visite' [] déjà_visité = déjà_visité 
          visite' (sommet:reste) déjà_visité = 
                if (sommet `elem` déjà_visité)
                then visite' reste déjà_visité
                else let ses_successeurs = (transitions graphe) sommet 
                         nouvelle_visite = visite' ses_successeurs (sommet:déjà_visité)
                     in visite' reste nouvelle_visite

{- | 'visiteCPS' est la version CPS de 'visite'. 
    On a que

    > visiteCPS t g s = t $ visite g s 

    c'est-à-dire, notamment que 

    > visiteCPS id g s = visite g s 
-} 
visiteCPS :: Eq a => ([a] -> t) -> Graph a -> a -> t 
visiteCPS = undefined 

{- $Q3
    un type de données très utilisées en programmation fonctionnelle est
    ce qu'on appelle les listes d'associations.  Ces listes associent une
    valeur à une clé. C'est une forme de dictionnaire et une implémentation
    simple (et peu efficace) des "maps" communes en informatique
    dont l'"hashmap" est l'implémentation la plus connue. 

    Nous représentons ici ces listes d'association par le type 'Dictionnaire'.
    Ce type prend des clés et des valeurs pour les associer dans une liste.
    Vous devez implémenter, sous forme CPS, les fonctions @chercherCPS@,
    @chercherTousCPS@ et @insérerCPS@.

    Les implémentations classiques de celles-ci sont données dans le module 
    'Data.Dictionnaire' disponible pour le TP. 
-}

chercherCPS :: Eq c => (Maybe v -> t) -> Dictionnaire c v -> c -> t 
chercherCPS = undefined 

chercherTousCPS :: Eq c => ([v] -> t) -> Dictionnaire c v -> c -> t
chercherTousCPS = undefined 

insérerCPS :: Eq c => (Dictionnaire c v -> t) -> Dictionnaire c v -> c -> v -> t 
insérerCPS = undefined 

{- $Q4
    Les continuations permettent d'avoir un pouvoir important sur le contrôle
    de flux des données dans un programme. Cependant, le typage et la lisibilité
    souffrent grandement d'une utilisation accrue des continuations sauf lors
    d'utilisation ponctuelle. 

    Pour améliorer ça, on peut développer un type de données @'Continuation' t a@.
    On peut profiter du fait que les calculs par continuation ont une structure
    de monade pour fournir des implémentations plus agréable et alléger ainsi
    le code et la lisibilité des fonctions implémentés en style CPS.

    Pour cette question, plusieurs fonctions sont fournis sous la forme CPS
    à l'aide de la monade Continuation (dont l'implémentation est aussi fournie)
    et vous devez, quant à vous, implémenter le code des fonctions 
    'chercher'', 'chercherTous'', 'insérer'' et 'visite'' qui correspondent
    aux versions des fonctions 'chercherCPS', 'chercherTousCPS', 'insérerCPS' 
    et 'visiteCPS' utilisant le nouveau type de données 'Continuation'. 
-}

data Continuation t a = Continuation { 
    {- | Exécute un calcul sous forme CPS en retournant son résultat après avoir
         appliqué la continuation finale, c'est-à-dire la continuation à lancer 
         lorsque le calcul est terminé.
         Les paramètres sont

         * le calcul produisant un @a@ avec l'attente de sa continuation (@Continuation@)

         * la continuation finale à appliquer une fois le calcul effectué (souvent 'id') 
    -}
    exécuter :: (a -> t) -> t 
}

instance Functor (Continuation t) where

    -- fmap :: (a->b) -> Continuation t a -> Continuation t b 
    fmap f x  = Continuation $ \c -> exécuter x $ \a -> c (f a)
    -- c :: (b -> t)
    -- exécuter :: Continuation t a -> (a -> t) -> t 
    -- x :: Continuation t a
    -- a :: a
    -- \c -> ... :: (b -> t) -> t
    -- \a -> ... a -> t
    -- f :: a -> b
    -- f a :: b

instance Applicative (Continuation t) where
    -- pure :: a -> Continuation t a 
    pure a = Continuation ($ a)

    -- (<*>) :: Continuation t (a->b) -> Continuation t a -> Continuation t b 
    f <*> x  = Continuation $ \c -> exécuter f $ \f' -> exécuter (fmap f' x) c
    -- c :: (b -> t)
    -- f :: Continuation t (a->b)
    -- exécuter1 :: Continuation t (a->b) -> ((a->b) -> t) -> t 
    -- f' :: (a->b)
    -- \f' -> ... (a->b) -> t
    -- fmap f' x :: Continuation t b
    -- \c -> ... :: (b -> t) -> t
    -- exécuter2 :: Continuation t b -> (b -> t) -> t 

instance Monad (Continuation t) where
    -- return :: a -> Continuation t a 
    -- return = pure - redondant

    -- (>>=) :: Continuation t a -> (a -> Continuation t b) -> Continuation t b 
    m >>= k  = Continuation $ \c -> exécuter m $ \a -> exécuter (k a) c
    -- c :: (b -> t)
    -- m :: Continuation t a
    -- k :: (a -> Continuation t b)
    -- a :: a
    -- \c -> ... :: (b -> t) -> t
    -- \a -> ... a -> t
    -- exécuter1 :: Continuation t a -> (a -> t) -> t 
    -- exécuter2 :: Continuation t b -> (b -> t) -> t 

map' :: (a -> b) -> [a] -> Continuation t [b] 
map' f = return . map f  

filter' :: (a -> Bool) -> [a] -> Continuation t [a] 
filter' p = return . filter p

produit :: [Integer] -> Continuation t Integer
produit [] = return 1
produit (0:_) = return 0 
produit (x:xs) = produit xs >>= return . (x*) 

carre :: [Integer] -> Continuation t [Integer]
carre xs = return $ map (^2) xs 

produitCarre :: [Integer] -> Continuation t Integer 
produitCarre xs = carre xs >>= produit 

reverse' :: [a] -> Continuation t [a] 
reverse' = return . reverse 

transitions' :: Graph a -> a -> Continuation t [a]
transitions' g = return . transitions g

-- fonctions à implémenter avec le type Continuation : 
-- Pensez à utiliser savamment les fonctions préalablement définies.

chercher' ::  Eq c => Dictionnaire c v -> c -> Continuation t (Maybe v)
chercher' = undefined 

chercherTous' :: Eq c => Dictionnaire c v -> c -> Continuation t [v] 
chercherTous' = undefined 

insérer' :: Eq c => Dictionnaire c v -> c -> v -> Continuation t (Dictionnaire c v) 
insérer' = undefined 

visite' :: Eq a => Graph a -> a -> Continuation t [a] 
visite' graphe s0 = visite'' [s0] [] >>= reverse'
    where visite'' = undefined 

{-
   Exemples et tests 
-}

testsQ1 = do mapCPS (filterCPS print even) (3*) [1,2,3,4,5,6,7]
             mapCPS (filterCPS print (<7)) (2*) [1,2,3,4,5,6,7]
             mapCPS (filterCPS (print . reverse) (>5)) (2*) [1,2,3,4,5,6,7]

testsQ2 = let g = G t 
              -- définition de la fonction de transition t
              t 1 = [2]
              t 2 = [3,4]
              t 3 = [4]
              t 4 = [4]
              t _ = []
              -- continuation finale 
              k n = print . (== sort (visite g n)) . sort
          in mapM_ (\n -> visiteCPS (k n) g n) [1..4]

testsQ3 = let d = [(1,'a'), (2,'b'), (1,'c'), (3,'d'), (2,'d')] 
          in do chercherCPS print d 1
                chercherCPS print d 2
                chercherTousCPS print d 1
                chercherTousCPS print d 2
                insérerCPS (\ d -> chercherTousCPS print d 2) d 2 'e' 

testsQ4 = let g = G t 
              -- définition de la fonction de transition t
              t 1 = [2]
              t 2 = [3,4]
              t 3 = [4]
              t 4 = [4]
              t _ = []
              -- continuation finale pour la visite 
              k n = print . (== sort (visite g n)) . sort
              -- dictionnaire 
              d = [(1,'a'), (2,'b'), (1,'c'), (3,'d'), (2,'d')] 
          in do putStrLn "-- Tests sur la visite de graphe"
                exécuter (visite' g 1) print
                exécuter (visite' g 1 >>= produit) print
                exécuter (visite' g 1 >>= carre >>= produit) (print . (==576)) 
                putStrLn "-- Tests sur les dictionnaires"
                exécuter (chercher' d 1) print
                exécuter (chercher' d 2) print
                exécuter (chercherTous' d 1) print
                exécuter (chercherTous' d 2) print
                exécuter (insérer' d 2 'e' >>= \ d -> chercherTous' d 2) print 


