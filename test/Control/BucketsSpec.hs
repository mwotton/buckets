{-# LANGUAGE ScopedTypeVariables #-}
module Control.BucketsSpec where

import           Control.Applicative ((<$>))
import           Control.Buckets
import           Data.IORef          (readIORef)
import           Data.IORef          (newIORef)
import           Data.IORef          (atomicModifyIORef')
import           Data.List           (sort)
import           Data.List           (nub)
import           Data.Monoid         ((<>))
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

buildBackend :: Eq a => Int -> [a] -> IO (IO (Event String [a]), t -> [[Integer]] -> IO (), IO [Integer])
buildBackend n items = do
  inRef <- newIORef items
  outRef <- newIORef ([]::[Integer])
  return ( do
              r <- atomicModifyIORef' inRef (\x -> (drop n x, take n x))
              return $ if r == []
                       then Quit
                       else Entries "hello" r
         , \k newItems -> atomicModifyIORef' outRef (\x -> (x <> concat newItems, ()))

         , readIORef outRef)

-- we nub because we don't care how many times we get an individual entry.

spec :: Spec
spec = describe "buckets" $ do
  it "doesn't leak in a normal case" $ do
    (fetch,dispose, get) <- buildBackend 3 [1..10]
    bucket 2 fetch dispose
    (nub . sort <$> get) `shouldReturn` nub (sort [1..10])
  it "does not leak" $ property $ \(x::[Integer],  Positive i, Positive n ) -> do
    (fetch,dispose, get) <- buildBackend n x
    bucket i fetch dispose
    (nub . sort <$> get) `shouldReturn` nub (sort x)
