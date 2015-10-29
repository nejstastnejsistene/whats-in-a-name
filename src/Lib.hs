{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import Control.Monad.IO.Class
import Control.Monad.Random (getRandomR)
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList, (!))
import System.Environment
import Web.Spock

main :: IO ()
main = do
  port <- fromMaybe 3030
      <$> fmap read
      <$> lookupEnv "PORT"
  dict <- fromList
      <$> map (\x -> if T.all isLower x then T.toTitle x else x)
      <$> filter (T.all isAlpha)
      <$> T.words
      <$> T.readFile "data/words.txt"
  runServer port dict

runServer :: Int -> Vector T.Text -> IO ()
runServer port dict = runSpock port $ spockT id $ do
  get root $ do
    setHeader "Access-Control-Allow-Origin" "*"
    text =<< liftIO (randName 2)
  where
    randName n = T.unwords <$> randWords n
    randWords n = sequence (replicate n randWord)
    randWord = (dict !) <$> getRandomR (0, length dict - 1)
