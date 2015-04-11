module Types
    ( Request()
    , Action(..)
    , AppState(..)
    , SnippetRecord()
    , Snippet(..)
    , NewSnippetRecord()
    , NewSnippet(..), emptyNewSnippet
    , State(..), loading, allSnippets, oneSnippet, error, creatingSnippet
    ) where

import Data.Foreign
import Data.Foreign.Class
import Control.Monad.Maybe.Trans
import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Control.Monad.Trans

type Request eff a = MaybeT (ContT Unit (Eff eff)) a

-- Full Snippet
type SnippetRecord = { id    :: Number
                     , title :: String
                     , code  :: String
                     , sucks :: Number
                     , rocks :: Number
                     }

data Snippet = Snippet SnippetRecord

instance snippetIsForeign :: IsForeign Snippet where
    read value = do
        id    <- readProp "id"    value
        title <- readProp "title" value
        code  <- readProp "code"  value
        sucks <- readProp "sucks" value
        rocks <- readProp "rocks" value
        return $ Snippet { id: id, title: title, code: code, sucks: sucks, rocks: rocks }

-- User Created Snippet
type NewSnippetRecord = { title :: String
                        , code  :: String
                        }

data NewSnippet = NewSnippet NewSnippetRecord

emptyNewSnippet :: NewSnippet
emptyNewSnippet = NewSnippet { title: "", code: "" }

data Action = LoadAllSnippets
            | LoadSnippet Number
            | VoteSnippetRocks Snippet
            | VoteSnippetSucks Snippet
            | OpenNewSnippet
            | PostNewSnippet NewSnippet
            | EditNewSnippet NewSnippet

data AppState = Loading
              | AllSnippets [Snippet]
              | OneSnippet Snippet
              | Error
              | CreatingSnippet NewSnippet

data State = State AppState

loading :: State
loading = State Loading

allSnippets :: [Snippet] -> State
allSnippets = State <<< AllSnippets

oneSnippet :: Snippet -> State
oneSnippet = State <<< OneSnippet

error :: State
error = State $ Error

creatingSnippet :: NewSnippet -> State
creatingSnippet = State <<< CreatingSnippet
