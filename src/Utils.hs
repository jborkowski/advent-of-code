module Utils (load) where

load :: (Read a) => String -> IO [a]
load path = do
  content <- readFile path
  let ls = lines content
      as = read <$> ls
  pure as
