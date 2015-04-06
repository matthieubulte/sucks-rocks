module Types
    ( Action(..)
    , AppState(..)
    , Snippet(..)
    , State(..), loading, allSnippets, oneSnippet
    ) where

import Data.Foreign
import Data.Foreign.Class

data Snippet = Snippet { id    :: Number
                       , title :: String
                       , code  :: String
                       , sucks :: Number
                       , rocks :: Number
                       }

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

data State = State AppState

loading :: State
loading = State Loading

allSnippets :: [Snippet] -> State
allSnippets = State <<< AllSnippets

oneSnippet :: Snippet -> State
oneSnippet = State <<< OneSnippet
