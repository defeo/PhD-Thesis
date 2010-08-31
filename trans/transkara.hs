-- \begin{lstlisting}[language=haskell,basicstyle=\ttfamily\footnotesize]

import Data.TypeLevel hiding (Mul)
import Data.Param.FSVec
import qualified Prelude as P

------ The ring

type R = P.Int
zero :: R
zero = 0::P.Int
plus :: R -> R -> R
plus = (P.+)
times :: R -> R -> R
times = (P.*)

------ Additive circuits

class Category (~>) where
  id :: Nat a => a -> (a ~> a)
  (.) :: (Nat a, Nat b, Nat c) => (b ~> c) -> (a ~> b) -> (a ~> c)

class Category (~>) => AbCategory (~>) where
  zeroArrow :: (Nat a, Nat b) => a -> b -> (a ~> b)
  (<+>) :: (Nat a, Nat b) => (a ~> b) -> (a ~> b) -> (a ~> b)

class AbCategory (~>) => AdditiveCategory (~>) where
  (&&&) :: (Nat a, Nat b, Nat c, Add b c bc,
            Min bc b b)
           => (a ~> b) -> (a ~> c) -> (a ~> bc)
  (|||) :: (Nat a, Nat b, Nat c, Add a b ab,
            Min ab a a)
           => (a ~> c) -> (b ~> c) -> (ab ~> c)
  (***) :: (Nat a, Nat b, Nat c, Nat d, Add a c ac, Add b d bd,
            Min ac a a, Min bd b b)
           => (a ~> b) -> (c ~> d) -> (ac ~> bd)
  
------ Bicategories

data Cat a b = Cat a b 
               (FSVec a R -> FSVec b R) 
               (FSVec b R -> FSVec a R)

apply :: Cat a b -> FSVec a R -> FSVec b R
apply (Cat n m f g) x = f x
transapply :: Cat a b -> FSVec b R -> FSVec a R
transapply (Cat n m f g) x = g x

instance Category Cat where
  id n = Cat n n
         (\x -> x)
         (\x -> x)
  (Cat n m f g) . (Cat n' m' f' g') = Cat n' m
                                      (\x -> f (f' x))
                                      (\x -> g' (g x))
  
instance AbCategory Cat where
  zeroArrow n m = Cat n m 
                  (\x -> iterate m (\x -> zero) zero)
                  (\x -> iterate n (\x -> zero) zero)
  (Cat n m f g) <+> 
    (Cat n' m' f' g') = Cat n m
                        (\x -> zipWith plus (f x) (f' x))
                        (\x -> zipWith plus (g x) (g' x))
  
instance AdditiveCategory Cat where
  (Cat n m f g) &&& (Cat n' m' f' g') =
    Cat n (m + m')
    (\x -> (f x) ++ (f' x))
    (\x -> zipWith plus (g (take m x)) (g' (drop m x)))
  (Cat n m f g) ||| (Cat n' m' f' g') =
    Cat (n + n') m
    (\x -> zipWith plus (f (take n x)) (f' (drop n x)))
    (\x -> (g x) ++ (g' x))
  (Cat n m f g) *** 
    (Cat n' m' f' g') = Cat (n + n') (m + m') 
                        (\x -> f (take n x) ++ f' (drop n x))
                        (\x -> g (take m x) ++ g' (drop m x))

-- injections
scalar r = Cat d1 d1
           (\x -> singleton (times (head x) r))
           (\x -> singleton (times (head x) r))

---- Scalar multiplication

-- HList hacks, following Kiselyov and Peyton-Jones:
-- http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap
-- instead of ShowPred as in the online article, we use Trich as
-- provided by Data.Typelevel

class Nat a => SMul a where
  smul :: R -> Cat a a     

class Nat a => SMul' flag a where 
  smul' :: flag -> R -> Cat a a 

-- the instantiations of SMul'
instance SMul' EQ D0 where
  smul' _ x = zeroArrow d0 d0
  
instance (Nat b, Succ b a, Trich b D0 f, 
          SMul' f b, Add b D1 a, Min a D1 D1) 
         => SMul' GT a where 
  smul' _ x = (scalar x) *** 
              ((smul'::f -> R -> Cat b b) P.undefined x)

-- and finally the instantiation of SMul !!
instance (Trich a D0 flag, SMul' flag a) => SMul a where
  smul = smul' (P.undefined::flag)


---- Full multiplication

class (Nat preca, Succ preca a, Nat b, Add preca b c) 
      => Mul preca a b c where
  mul :: FSVec a R -> Cat b c

class (Nat preca, Succ preca a, Nat b, Add preca b c) 
      => Mul' flag preca a b c where 
  mul' :: flag -> FSVec a R -> Cat b c

-- the instantiations of Mul'
instance (Nat b, Add D0 b b, Trich b D0 f, SMul' f b) 
         => Mul' EQ D0 D1 b b where
  mul' _ x = smul (head x) 

instance (Nat ppa, Succ ppa pa, Succ pa a, Nat b, 
          Add ppa b pc, Add pa b c,
          Trich ppa D0 f, Mul' f ppa pa b pc,
          Trich b D0 f', SMul' f' b,
          Add pc D1 c, Min c D1 D1, Min b D0 D0,
          Add b D0 b, Min b b b, Min c b b)
         => Mul' GT pa a b c where
  mul' _ x = (zeroArrow d0 d1 ***
              (mul'::f -> FSVec pa R -> Cat b pc) 
              P.undefined (tail x))
             <+>
             ((smul::R -> Cat b b) (head x) ***
              zeroArrow P.undefined P.undefined)

-- and finally the instantiation of Mul !!
instance (Trich pa D0 flag, Mul' flag pa a b ab) 
         => Mul pa a b ab where
  mul = mul' (P.undefined::flag)

-- Run ghci -fglasgow-exts -XUndecidableInstances
-- then try (for example)
--   ghci> apply (mul (1 +> 2 +> empty)) (1 +> 2 +> 45 +> 10 +> empty)
--   ghci> transapply (mul (1 +> 2 +> empty)) (1 +> 2 +> 45 +> 10 +> empty)
-- enjoy \end{lstlisting}\endinput
