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
