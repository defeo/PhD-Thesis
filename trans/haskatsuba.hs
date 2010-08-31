-- \begin{lstlisting}[language=haskell,basicstyle=\ttfamily\small]

-- Linear and Scalar wrappers
data L = Lin Integer
data S = Sca Integer

-- The ring
class Ring r where
  zero :: r
  (<+>) :: r -> r -> r
  (<*>) :: r -> S -> r
  neg :: r -> r
  
-- Free modules
class Ring r => Module m r | m -> r where
  zeroM :: m
  (<<*) :: m -> S -> m
  (>>>) :: m -> Integer -> r
  (<<<) :: r -> Integer -> m
  (<++>) :: m -> m -> m
  add :: m -> m -> Integer -> m
  add a b n = foldl (<++>) zeroM 
              [((a>>>i) <+> (b>>>i))<<<i | i <- [1..n]]
  
-- Linear is an instance of Ring
instance Ring L where
  zero = Lin 0
  (Lin x) <+> (Lin y) = Lin (x+y)
  (Lin x) <*> (Sca y) = Lin (x*y)
  neg (Lin x) = Lin (-x)
  
-- Scalar is an instance of Ring
instance Ring S where
  zero = Sca 0
  (Sca x) <+> (Sca y) = Sca (x+y)  
  (Sca x) <*> (Sca y) = Sca (x*y)
  neg (Sca x) = Sca (-x)

-- We can add any other constant we like to S
one = Sca 1

-- Lists (polynomials) are free modules
instance Ring r => Module [r] r where
  zeroM = [zero]
  [] <<* x = []
  (x:xs) <<* y = (x <*> y):(xs <<* y)
  [] >>> i = zero
  (x:xs) >>> i =
    if i < 1
    then zero 
    else if i == 1 
         then x 
         else xs >>> (i-1)
  x <<< i = if i <= 1 then [x] else zero:(x <<< (i-1))
  [] <++> [] = []
  (x:xs) <++> [] = x:(xs <++> [])
  [] <++> (y:ys) = y:([] <++> ys)
  (x:xs) <++> (y:ys) = (x <+> y):(xs <++> ys)
  add [] [] n = []
  add [] (y:ys) n =
    if n > 0 then y:(add [] ys (n-1)) else []
  add (x:xs) [] n =
    if n > 0 then x:(add xs [] (n-1)) else []
  add (x:xs) (y:ys) n =
    if n > 0 then (x<+>y):(add xs ys (n-1)) else []


-- Karatsuba multiplication : the system will infer
-- shift :: Ring r => [r] -> Int -> [r]
-- split :: Ring r => [r] -> Int -> ([r], [r])
-- kara :: Ring r => [r] -> [S] -> Int -> [r]

shift x n = if n <= 0 then x else shift (zero:x) (n-1)

split [] n = ([], [])
split (x:xs) n =
  if n <= 0
  then ([], x:xs)
  else let (a, b) = split xs (n-1) in (x:a, b)

kara [] y n = []
kara x [] n = []
kara x y n =
  if n <= 0
  then []
  else if n == 1 
       then [(x!!0) <*> (y!!0)]
       else 
         let h = n `div` 2 in
         let (a0, a1) = split x h in
         let (b0, b1) = split y h in
         let x0 = kara a0 b0 h in
         let x2 = kara a1 b1 (n-h) in
         let xx1 = kara (a1 <++> a0) (b1 <++> b0) (n-h) in
         let x1 = xx1 <++> ((x0 <++> x2) <<* (neg one)) in
         (shift x2 n) <++> (shift x1 h) <++> x0
         
-- Run ghci -fglasgow-exts
-- and at the prompt type 
--    ghci> :t kara
-- enjoy \end{lstlisting}\endinput
