module Matrix 

import Vector 
import Finite 

public export 
Matrix : Type -> Nat -> Nat -> Type
Matrix a m n = Vec (Vec a n) m

export 
insert : Matrix a m n -> Fin m -> Fin n -> a -> Matrix a m n
insert (v :: vs) FZ iy new = (Vector.insert v iy new) :: vs  
insert (v :: vs) (FS ix) iy new = v :: (insert vs ix iy new)  

export 
getAt : Matrix a m n -> Fin m -> Fin n -> a 
getAt vs fm fn = Vector.getAt (Vector.getAt vs fm) fn 

export 
transpose : {m : Nat} -> {n : Nat} -> Matrix a m n -> Matrix a n m 
transpose {n = Z} _ = Nil
transpose {m = Z} _ = Vector.replicate Nil n
transpose {m = S j} {n = S k} mat = (Vector.map (\v => Vector.getAt v FZ) mat) :: (transpose (Vector.map Vector.tail mat))

export 
rows : Matrix a m n -> Matrix a m n 
rows = id 

export 
cols : {m : Nat} -> {n : Nat} -> Matrix a m n -> Matrix a n m 
cols = transpose

export 
diagonal : {m : Nat} -> Matrix a (S m) (S m) -> Vec a (S m) 
diagonal {m = Z} (x :: _) = (Vector.head x) :: Nil 
diagonal {m = S j} (x :: xs) = (Vector.head x) :: (diagonal (Vector.map Vector.tail xs)) 

export 
diagonals : {m : Nat} -> Matrix a (S m) (S m) -> Matrix a 2 (S m)
diagonals mat = (diagonal mat) :: (diagonal $ transpose mat) :: Nil