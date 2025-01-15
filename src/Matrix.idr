module Matrix 

import Vector 

public export 
Matrix : Type -> Nat -> Nat -> Type
Matrix a m n = Vec (Vec a n) m

 
