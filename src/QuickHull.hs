-- Define an abstract Point class
class Functor p => Point p where
    origin          :: Num a => p a
    (<+>)           :: Num a => p a -> p a -> p a
    (<->)           :: Num a => p a -> p a -> p a
    x <+> y         = x <+> (negateP y)
    x <-> y         = x <-> (negateP y)
    negateP         :: Num a => p a -> p a
    negateP         = fmap ((-1)*)
    (<.>)           :: Num a => p a -> p a -> a
    (<*>)           :: Num a => a -> p a -> p a
    k <*> p         = fmap (k*) p

-- Define the Point3 type
data Point3 a = P3 a a a deriving (Eq, Show)

-- Point3 is an instance of Functor
instance Functor Point3 where
    fmap f (P3 x y z) = P3 (f x) (f y) (f z)

-- Point3 is an instance of Point
instance Point Point3 where
    origin                      = P3 0 0 0
    P3 x y z <+> P3 x' y' z'    = P3 (x + x') (y + y') (z + z')
    P3 x y z <.> P3 x' y' z'    = x*x' + y*y' + z*z'

cross3 :: Num a => Point3 a -> Point3 a -> Point3 a
P3 x y z `cross3` P3 x' y' z' =
    P3 (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')
