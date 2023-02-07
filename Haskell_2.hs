{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-| TP4 du cours IFT359 à remettre le 13 novembre 2022

    Remis par :
-}
module TP4 
    (
    -- * Question 1 : un monoid 
    -- $question1
      Min (..)
    , minimum 
    -- * Question 2 : instances de classes pour un arbre binaire
    -- $question2
    , BTree (..) 
    , root
    , leaf
    , node
    , left
    , right
    , isLeaf
    , isNode
    , btree
    -- * Question 3 : Structure de file
    -- $question3
    , File (..)
    , vide
    , estVide
    , premier
    , reste
    , (<+)
    , (<+<)
    , renverse
    , taille 
    ) where

import Control.Applicative
import Data.List (foldl')
import Data.Monoid 
import qualified Prelude (minimum) 
import Prelude hiding (minimum)
-- ces deux lignes font que l'appel à la fonction @minimum@ du 'Prelude', 
-- se fait par 'Prelude.minimum'. De cette façon, l'appel à lafonction 
-- 'minimum' définie ici se fait sans ajout du préfixe @TP4@

{- $question1
   Monoid des minimums 

   Implémenter les instances de 'Functor' et de 'Applicative' pour le monoid 'Min'. 

   On rappelle que les propriétés suivantes doivent être vraies :
   
   >> -- Functor
   >> fmap id  ==  id
   >> fmap (f . g)  ==  fmap f . fmap g

   >> -- Applicative
   >> pure id <*> v = v
   >> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   >> pure f <*> pure x = pure (f x)
   >> u <*> pure y = pure ($ y) <*> u

   >> -- Monoid 
   >> mappend mempty x = x
   >> mappend x mempty = x
   >> mappend x (mappend y z) = mappend (mappend x y) z
   >> mconcat = 'foldr' mappend mempty
-}
newtype Min a = Min { getMin :: a }
    deriving (Show)

instance Functor Min where
    fmap func (Min val) = Min (func val) 

instance Applicative Min where
    pure = Min
    Min f <*> j = fmap f j 
  

instance (Bounded a, Ord a) => Monoid (Min a) where 
    mempty = Min maxBound
  --mappend = '(<>)' by default

instance (Bounded a, Ord a) => Semigroup (Min a) where
    (<>) = liftA2 min 

minimum :: (Bounded a, Ord a) => [a] -> a 
minimum = getMin . mconcat . map Min 

{- $question2
   Avec la définition de la structure de donnée algébrique 'ArbreBinaire' qui vous est donnée, 
   instancier les classes 
   
    * 'Functor', c'est-à-dire la méthode 'fmap';

    * 'Applicative', c'est-à-dire les méthodes 'pure' et '(<*>)';

    * 'Monad', c'est-à-dire la méthode '(>>=)' (la méthode 'pure' correspond à la méthode 'pure'
      de la classe 'Applicative';
      
   On rappelle que les propriétés suivantes doivent être vraies :
   
   >> -- Functor
   >> fmap id  ==  id
   >> fmap (f . g)  ==  fmap f . fmap g

   >> -- Applicative
   >> pure id <*> v = v
   >> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   >> pure f <*> pure x = pure (f x)
   >> u <*> pure y = pure ($ y) <*> u

   >> -- Monad 
   >> return a >>= k  ==  k a
   >> m >>= return  ==  m
   >> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
   >> fmap f xs  ==  xs >>= return . f
-}

data BTree b a = Leaf a | Node b (BTree b a) (BTree b a)
    deriving (Show, Read, Eq, Ord)

root :: BTree b a -> Either b a
root (Leaf a) = Right a 
root (Node b _ _) = Left b    
    
leaf :: BTree b a -> a
leaf (Leaf a) = a 
leaf (Node b _ _) = error " doesnt exist"

node :: BTree b a -> b
node (Node b _ _) = b
node (Leaf a) = error " doesnt exist " 

left :: BTree b a -> BTree b a 
left (Node b val val2 ) =  val 

right :: BTree b a -> BTree b a 
right (Node b val1 val2 ) = val2


isLeaf :: BTree b a -> Bool
isLeaf (Leaf a) = True 
isLeaf (Node b _ _) = False 


isNode :: BTree b a -> Bool
isNode (Node b _ _) = True
isNode (Leaf a) = False 

btree :: (b -> c) -> (a -> c) -> BTree b a -> c 
btree f1 f2 (Leaf a) = f2 a
btree f1 f2 (Node b _ _) = f1 b   

instance Functor (BTree b) where
    fmap f ( Leaf a) = Leaf (f a)
    fmap f ( Node b left right) = Node b (fmap f left ) (fmap f right) 

instance Applicative (BTree b) where 
    pure = Leaf
    Leaf f <*> x = fmap f x
    Node b left right <*> t = Node b (left <*> t) (right <*> t)

instance Monad (BTree b) where 
    return = pure 
    Leaf a >>= f = f a 
    Node b left right >>= f = Node b (left >>=f) ( right >>=f)

{- $question3
   Implémenter les fonctions utilisant la structure de donnée algébrique 'File'.
   Cette strcture implémente une file de données, c'est-à-dire une structure
   de donnée du type FIFO (premier entré, premier sorti). L'utilisation d'une 
   liste est la façon la plus simple et la plus évidente pour représenter cette
   structure. 

   Implémenter ensuite les fonctions indiquées dans la suite pour cette structure. 
-}

data File a = File {f :: [a] } -- à faire 
    deriving (Eq, Ord, Show, Read) 

{- | @'vide'@ est la file vide -}
vide :: File a
vide = File [] 


{- | @'estVide' xs@ retourne 'True' si la file @xs@ est vide; 'False' sinon. 
   On a les propriétés suivantes :

   >> estVide vide = True
   >> estVide $ ajoute xs x = False 
-}
estVide :: File a -> Bool
estVide (File []) = True
estVide (File (x:xs)) = False 

{- | @'premier' xs@ retourne le premier élément en tête de la liste @xs@.
   Si @xs@ est vide, @'premier' xs@ retourne une erreur.
   On a les propriétés suivantes :

   >> premier $ vide = error "premier : file vide" 
-}
premier :: File a -> a
premier (File[]) = error "premier : file vide"
premier (File(x:xs)) = x 
{- | @'reste' xs@ retourne la file @x s@ privé de son premier élément.
   Si @xs@ est vide, @'reste' xs@ retourne une erreur.
   On a les propriétés suivantes :

   >> reste $ vide = error "reste : file vide" 
-}
reste :: File a -> File a
reste (File[]) = error " reste : file vide  "
reste  (File (x:xs)) = File xs

{- | @xs '<+' x@ ajoute l'élément @x@ à la fin de la file @xs@ 
   On a les propriétés suivantes :

   >> premier $ vide <+ x = x
   >> reste $ vide <+ x = vide
-}
(<+) :: File a -> a -> File a
(<+) (File a) x =  File ( a ++ [x])

{- | @xs '<+<' ys@ retourne la File constituée des éléments de @xs@ suivis 
   de ceux de @ys@. 
   On a les propriétés suivantes :

   >> vide <+< xs = xs
   >> xs <+< vide = xs
   >> premier $ xs <+< ys = premier xs
   >> reste $ xs <+< ys = reste xs <+< ys 
   >> (xs <+< ys) <+ z = xs <+< (ys <+ z) 
-}
(<+<) :: File a -> File a -> File a
(<+<)  (File a) (File y) = File ( a ++ y)

{- | @'renverse' xs@ retourne la file des éléments de @xs@ disposés dans le 
   sens inverse de ceux de @xs@
   On a les propriétés suivantes :

   >> renverse vide = vide
   >> premier $ renverse $ xs <+ x = x  
   >> reste $ renverse $ xs <+ x = renverse xs 
   >> renverse $ xs <+< ys = renverse ys <+< renverse xs
-}
renverse :: File a -> File a
renverse a  =  File (reverse (f a))  

{- | @'taille' xs@ retourne la taille de la file @xs@ 
   On a les propriétés suivantes :

   >> taille vide = 0
   >> taille $ xs <+ x = taille xs + 1
   >> taille $ reste xs = taille xs - 1
   >> taille $ xs <+< ys = taille xs + taille ys 
   >> taille $ renverse xs = taille xs 
-}
taille :: File a -> Int 
taille a = length (f a) 

