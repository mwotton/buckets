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

bucket :: (Hashable a1, Hashable k, Eq a1, Eq k) => Int -> IO (Event k [a1]) -> (k -> [[a1]] -> IO a) -> IO ()
bucket maxEntries fetch dispose = go HM.empty
  where go dict = do
          res <- fetch
          case res of
            Quit -> flush dict >> putStrLn "quitting"
            Flush -> flush dict >> putStrLn "flush called" >> go HM.empty
            Entries tag entry -> do
              newDict <- case HM.lookup tag dict of
                Nothing -> return $ HM.insert tag (maxEntries, HS.singleton entry) dict
                Just (n,entries) ->
                  if n <= 0
                  then do dispose tag (HS.toList entries)
                          return (HM.insert tag (maxEntries,HS.empty) dict)
                  else return $ HM.insert tag (n-(length entry),(HS.insert entry entries)) dict
              go newDict

        flush dict = do
          void $ sequence $ HM.foldlWithKey' (\actions k (_,v) -> (dispose k (HS.toList v)):actions) [] dict
          print "flushed"