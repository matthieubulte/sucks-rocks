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
import Control.Monad.Trans.Reader
import Data.Aeson (object, (.=), Value)
import Data.Functor
import Database.Persist as P
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.HTTP.Types
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment (getArgs)
import Web.Scotty as S

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


mkSnippetId :: Int -> SnippetId
mkSnippetId = toSqlKey . fromIntegral


getSnippets :: Int -> Int -> IO [Entity Snippet]
getSnippets l p = runDb $ selectList [] [LimitTo l, OffsetBy (l * (p-1))]


getSnippetById :: SnippetId -> IO (Maybe (Entity Snippet))
getSnippetById i = do
    snippet <- runDb $ selectList [SnippetId ==. i] [LimitTo 1]
    return $ case snippet of
                  ([e]) -> Just e
                  _     -> Nothing


serve :: Int -> IO ()
serve port = S.scotty port $ do

    S.middleware . staticPolicy $ addBase "static"
    S.middleware . staticPolicy $ addBase "bower_components"

    S.get "/" $ S.file "static/index.html"

    S.get "/snippets" $ do
      snippets <- liftIO $ getSnippets 10 0
      S.json $ snippetEntityToJson <$> snippets

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


populate :: ReaderT SqlBackend (ResourceT (NoLoggingT IO)) ()
populate = do
    _ <- insert $ Snippet "title 1" "code 1" 0 0
    _ <- insert $ Snippet "title 2" "code 2" 0 0
    _ <- insert $ Snippet "title 3" "code 3" 0 0
    _ <- insert $ Snippet "title 4" "code 4" 0 0
    _ <- insert $ Snippet "title 5" "code 5" 0 0
    return ()

main :: IO ()
main = do
    runDb $ runMigration migrateAll
    runDb populate

    [port] <- getArgs
    serve (read port)
