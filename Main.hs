{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Aeson (object, (.=), (.:), decode, FromJSON(..), Value(..))
import Data.Functor
import Control.Applicative
import Database.Persist as P
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.HTTP.Types
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment (getArgs)
import Web.Scotty as S
import qualified Data.Text.Lazy as T

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT
      . runResourceT
      . withSqliteConn "dev.sqlite3"
      . runSqlConn


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Snippet
    title String
    code String
    sucks Int
    rocks Int
    deriving Show
|]


snippetEntityToJson :: Entity Snippet -> Value
snippetEntityToJson (Entity k s) = object [ "id"    .= fromSqlKey k
                                          , "title" .= snippetTitle s
                                          , "code"  .= snippetCode s
                                          , "rocks" .= snippetRocks s
                                          , "sucks" .= snippetSucks s
                                          ]

data NewSnippet = NewSnippet { title :: String
                             , code  :: String
                             }

instance FromJSON NewSnippet where
    parseJSON (Object v) = NewSnippet <$> v .: "title"
                                      <*> v .: "code"
    parseJSON _          = empty

mkSnippetId :: Int -> SnippetId
mkSnippetId = toSqlKey . fromIntegral


getSnippets :: IO [Entity Snippet]
getSnippets = runDb $ selectList [] []


getSnippetById :: SnippetId -> IO (Maybe (Entity Snippet))
getSnippetById i = do
    snippet <- runDb $ selectList [SnippetId ==. i] [LimitTo 1]
    return $ case snippet of
                  ([e]) -> Just e
                  _     -> Nothing


insertNewSnippet :: NewSnippet -> IO (Key Snippet)
insertNewSnippet s = let snippet = Snippet (title s) (code s) 0 0
                      in runDb $ insert snippet

serve :: Int -> IO ()
serve port = S.scotty port $ do

    S.middleware . staticPolicy $ addBase "static"
    S.middleware . staticPolicy $ addBase "bower_components"

    S.get "/" $ S.file "static/index.html"

    S.get "/snippets" $ do
      snippets <- liftIO getSnippets
      S.json $ snippetEntityToJson <$> snippets

    S.post "/snippet" $ do
        reqBody <- body
        let newSnippet = decode reqBody :: Maybe NewSnippet

        case newSnippet of
             Nothing -> status status400
             Just s -> do
                 _id <- liftIO $ insertNewSnippet s
                 status status200
                 S.text . T.pack . show $ fromSqlKey _id


    S.put "/snippet/:id/rocks" $ do
        _id <- S.param "id"
        liftIO . runDb $ update (mkSnippetId _id) [SnippetRocks +=. 1]

    S.put "/snippet/:id/sucks" $ do
        _id <- S.param "id"
        liftIO . runDb $ update (mkSnippetId _id) [SnippetSucks +=. 1]

    S.get "/snippet/:id" $ do
        _id <- S.param "id"
        snippet <- liftIO $ getSnippetById (mkSnippetId _id)
        case snippet of
             Nothing  -> status status404
             (Just s) -> S.json $ snippetEntityToJson s


main :: IO ()
main = do
    runDb $ runMigration migrateAll

    [port] <- getArgs
    serve (read port)
