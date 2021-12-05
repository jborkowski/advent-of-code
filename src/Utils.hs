module Utils (load, load') where

load :: (Read a) => String -> IO [a]
load path = do
  ls <- load' path
  pure $ read <$> ls

load' :: String -> IO [String]
load' = fmap lines . readFile 
