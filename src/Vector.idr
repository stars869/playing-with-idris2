module Vector

import Finite 

public export 
data Vec : Type -> Nat -> Type where 
    Nil : Vec a Z 
    (::) : a -> Vec a n -> Vec a (S n)

export 
replicate : a -> (n : Nat) -> Vec a n 
replicate x Z = Nil 
replicate x (S m) = x :: (replicate x m)

export 
map : (a -> b) -> Vec a n -> Vec b n 
map f Nil = Nil 
map f (x :: xs) = (f x) :: (map f xs)

export 
showVec : (a -> String) -> String -> Vec a n -> String 
showVec showElement sep Nil = ""
showVec showElement sep (x :: Nil) = showElement x 
showVec showElement sep (x1 :: x2 :: xs ) = (showElement x1) ++ sep ++ (showVec showElement sep (x2 :: xs))

export 
insert : Vec a n -> Fin n -> a -> Vec a n 
insert (_ :: xs) FZ new = new :: xs 
insert (x :: xs) (FS ix) new = x :: (insert xs ix new)

export 
getAt : Vec a n -> Fin n -> a 
getAt (x :: xs) FZ = x 
getAt (x :: xs) (FS ix) = getAt xs ix 

export 
head : Vec a (S n) -> a 
head (x :: _) = x 

export 
tail : Vec a (S n) -> Vec a n
tail (_ :: xs) = xs

export 
all : Eq a => (a -> Bool) -> Vec a n -> Bool  
all f Nil = True 
all f (x :: xs) = if f x then (all f xs) else False 

export 
any : Eq a => (a -> Bool) -> Vec a n -> Bool 
any f Nil = False 
any f (x :: xs) = if f x then True else (any f xs) 

export
concat : Vec a m -> Vec a n -> Vec a (m + n)
concat Nil ys = ys 
concat (x :: xs) ys = x :: (concat xs ys)  
