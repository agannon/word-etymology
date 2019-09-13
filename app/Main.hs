{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Web.Spock
import           Web.Spock.Config

--JSON
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           Data.Text.Lazy   (toStrict)
import           GHC.Generics

--Persistent DB
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

import           Data.List.Split
import           Control.Monad.Reader

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Etymology json
  word Text
  frequencyRank Int
  langPath Text
  langBranch Text
  originLang Text
  originExample Text
  branchLang Text
  branchExample Text
  deriving Show
|]


type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  htmlString' <- readFile "views/home.html"
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg $ app $ pack htmlString')

app :: Text -> Api
app htmlFile = do

  -- below returns home page, which will be built out to show the Vue html
  get "home" $ html $ htmlFile


  -- below returns one word of the specified id number
  get ("api" <//> "words" <//> var) $ \etymologyId -> do
    maybeWord <- runSQL $ P.get etymologyId :: ApiAction (Maybe Etymology)
    case maybeWord of
      Nothing -> errorJson 2 "Could not find a word with that ID"
      Just theWord -> json theWord
      
  -- below filters based on keyword using SQL LIKE keyword
  -- or if there is no parameter, the whole list of words
  get ("api" <//> "words") $ do
    ps <- params
    let fil = case length ps of
                0 -> []
                _ -> case (Prelude.head (map fst ps)) of
                       "branch" -> [Filter EtymologyLangBranch
                                    (Left (Prelude.head (map snd ps))) (BackendSpecificFilter "LIKE")]
                       "origin" -> [Filter EtymologyOriginLang
                                    (Left (Prelude.head (map snd ps))) (BackendSpecificFilter "LIKE")]
                       "path" -> [Filter EtymologyLangPath
                                    (Left (Prelude.head (map snd ps))) (BackendSpecificFilter "LIKE")]
                       "blang" -> [Filter EtymologyBranchLang
                                    (Left (Prelude.head (map snd ps))) (BackendSpecificFilter "LIKE")]
                       _ -> []
    filteredWords <- runSQL $ selectList fil [Asc EtymologyFrequencyRank]
    json filteredWords

  --todo make post etc. raise 404

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error"  .= object ["code" .= code, "message" .=message]
    ]


-- IMPORT FUNCTIONS
parseData :: String -> [Etymology]
parseData contents = etymologies
  where
    (header:body) = lines contents
    body' = map init body
    tupleList = map (\x -> listToTuple8 $ splitOn "," x) body'
    etymologies = map (uncurry8 Etymology) tupleList


importData :: [Etymology] -> String -> IO [Key Etymology]
importData etymologies dbPath = do
  runSqlite (pack dbPath) . asSqlBackendReader $ forM etymologies insert


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

