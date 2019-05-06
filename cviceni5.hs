import Control.Monad (mapM_, when)

type Comments = [String]
data Commented a = Commented Comments a
  

{- vsechna undefined nahradte vlastni implementaci -}
comment :: String -> Commented ()
comment x = (Commented [x] ())

runCommented :: Commented a -> (a, [String])
runCommented (Commented comments value) = (value, comments)

instance Functor Commented where
  fmap f (Commented comments a) = Commented comments a' where a' = f a
  -- fmap :: (a->b) -> Commented a -> Commented b

instance Applicative Commented where
  -- pure :: a -> Commented a
  pure a = Commented [] a
  -- (<*>) :: Commented (a -> b) -> Commented a -> Commented b
  (Commented oldComments function) <*> (Commented newComments value) = Commented (oldComments ++ newComments) value' where value' = function value

instance Monad Commented where
  return = pure
  -- (>>=) :: Commented a -> (a -> Commented b) -> Commented b
  (Commented oldComments a) >>= f = let Commented newComments a' = f a in Commented (oldComments ++ newComments) a' 

{- Testovani -}
solutions a b c = do
  when (a == 0) $ comment "Ajaj, rovnice je ve skutecnosti linearni."
  let d = b ^ 2 - 4 * a * c
  comment $ "Diskriminant je " ++ show d
  if (d < 0)
    then do
      comment "Nemame reseni!"
      return []
    else do
      comment "Parada, mame alespon jedno reseni!"
      return $ (\op -> (-b `op` sqrt d) / (2 * a)) `map` [(+), (-)]

twoSolutions a b1 b2 c = do
  sol1 <- solutions a b1 c
  comment $ "Prvni rovnice ma " ++ show (length sol1) ++ " reseni"
  sol2 <- solutions a b2 c
  comment $ "Druha rovnice ma " ++ show (length sol1) ++ " reseni"
  return $ sol1 ++ sol2

{- jednoduchy komentovany foldr (negenericky, fungujici jen na seznamy) -}
cFoldr :: Show a => (a -> a -> a) -> a -> [a] -> Commented a
cFoldr func argument list = do
  if (length list == 0)
    then do
      return argument
    else do
      comment $ "Aplikujeme funkci, parametry: " ++ show argument ++ ", " ++ (show . last) list
      let result = func argument (last list)
      comment $ "Mezivysledek je: " ++ show argument
      cFoldr func result (init list)

main = do
  let (a, comments) = runCommented $ twoSolutions 5 (-3) (-4) (-1)
  mapM_ putStrLn comments
  putStrLn . ("Vysledek twoSolutions: " ++) . show $ a
  let (a, comments) = runCommented $ cFoldr (*) 1 [1 .. 10]
  mapM_ putStrLn comments
  putStrLn . ("Vysledek foldr: " ++) . show $ a
