{-# LANGUAGE ScopedTypeVariables #-}
module Control.Buckets where

import           Control.Monad       (void)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS

data Event k a = Entries !k !a
               | Flush
               | Quit

-- bucket :: (Hashable k, Num t, Monad m, Functor m, Eq t, Eq k) =>
--           t -> m (Event k a) -> (k -> [a] -> m ()) -> m ()
-- bucket :: (Hashable k, Num t, Eq t, Eq k) => t -> IO (Event k a1) -> (k -> [a1] -> IO a) -> IO ()


dumbBucket _foo fetch dispose = go
  where
    go = do
      r <- fetch
      case r of
        Quit -> return ()
        Flush -> go
        Entries tag entry -> do
          dispose tag [entry]
          go
bucket :: (Monad m, Functor m, Hashable a1, Hashable k, Eq a1, Eq k)
          => Int -> m (Event k [a1]) -> (k -> [[a1]] -> m ()) -> m ()
bucket maxEntries fetch dispose = go HM.empty
  where go dict = do
          res <- fetch
          case res of
            Quit -> flush dict --  >>  "quitting"
            Flush -> flush dict >>
                     -- putStrLn "flush called" >>
                     go HM.empty
            Entries tag entry -> do
              newDict <- case HM.lookup tag dict of
                Nothing -> return $ HM.insert tag (maxEntries, HS.singleton entry) dict
                Just (n,entries) ->
                  if n <= 0
                  then do void $ dispose tag (entry:(HS.toList entries))
                          return (HM.delete tag dict)
                  else return $ HM.insert tag (n-(length entry),(HS.insert entry entries)) dict
              go newDict

        flush dict = do
          void $ sequence $ HM.foldlWithKey' (\actions k (_,v) -> (dispose k (HS.toList v)):actions) [] dict
