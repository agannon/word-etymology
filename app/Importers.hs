{-# LANGUAGE OverloadedStrings #-}

module Importers ( parseData,
                  -- importData
                 )
  where

import Data.String
import Database.Persist.Sqlite
import Database.Persist

import Data.List.Split
import Control.Monad.Reader
import Data.Text (Text, pack)


data Etymology = Etymology {
  etymologyWord :: Text,
  etymologyFrequencyRank :: Int,
  etymologyLangPath :: Text,
  etymologyLangBranch :: Text,
  etymologyOriginLang :: Text,
  etymologyOriginExample :: Text,
  etymologyBranchLang :: Text,
  etymologyBranchExample :: Text
                           } deriving Show


parseData :: String -> [Etymology]
parseData contents = etymologies
  where
    (header:body) = lines contents
    body' = map init body
    tupleList = map (\x -> listToTuple8 $ splitOn "," x) body'
    etymologies = map (uncurry8 Etymology) tupleList


-- importData :: [Etymology] -> String -> IO [Key Etymology]
-- importData etymologies dbPath = do
--   runSqlite (pack dbPath) . asSqlBackendReader $ forM etymologies insert


asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id

uncurry8 func (a,b,c,d,e,f,g,h) = func a b c d e f g h

listToTuple8 [a,b,c,d,e,f,g,h] = (pack a
                                 ,read b :: Int
                                 ,pack c
                                 ,pack d
                                 ,pack e
                                 ,pack f
                                 ,pack g
                                 ,pack h)
