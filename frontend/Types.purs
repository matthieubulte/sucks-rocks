module Types
    ( Action(..)
    , AppState(..)
    , SnippetRecord()
    , Snippet(..)
    , State(..), loading, allSnippets, oneSnippet, snippetNotFound
    ) where

import Data.Foreign
import Data.Foreign.Class

type SnippetRecord =  { id    :: Number
                      , title :: String
                      , code  :: String
                      , sucks :: Number
                      , rocks :: Number
                      }

data Snippet = Snippet SnippetRecord

instance submissionIsForeign :: IsForeign Snippet where
    read value = do
        id    <- readProp "id"    value
        title <- readProp "title" value
        code  <- readProp "code"  value
        sucks <- readProp "sucks" value
        rocks <- readProp "rocks" value
        return $ Snippet { id: id, title: title, code: code, sucks: sucks, rocks: rocks }

data Action = LoadAllSnippets
            | LoadSnippet Number
            | VoteSnippetRocks Snippet
            | VoteSnippetSucks Snippet

data AppState = Loading
              | AllSnippets [Snippet]
              | OneSnippet Snippet
              | SnippetNotFound Number

data State = State AppState

loading :: State
loading = State Loading

allSnippets :: [Snippet] -> State
allSnippets = State <<< AllSnippets

oneSnippet :: Snippet -> State
oneSnippet = State <<< OneSnippet

snippetNotFound :: Number -> State
snippetNotFound = State <<< SnippetNotFound
