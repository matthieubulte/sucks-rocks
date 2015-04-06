module Api
    ( loadSnippet
    , loadSnippets
    , voteSnippetSucks
    , voteSnippetRocks
    ) where

import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Network
import Types

loadSnippet :: forall eff. Number -> ContT Unit (Eff eff) Snippet
loadSnippet id = do
    serializedSnippet <- get $ "/snippet/" ++ (show id)
    let deserializedSnippet = readJSON serializedSnippet :: F Snippet
    case deserializedSnippet of
         (Right snippet) -> return snippet
--       hu hu

loadSnippets :: forall eff. ContT Unit (Eff eff) [Snippet]
loadSnippets = do
    serializedSnippets <- get "/snippets"
    let deserializedSnippets = readJSON serializedSnippets :: F [Snippet]
    case deserializedSnippets of
         (Right snippets) -> return snippets
         (Left _)         -> return []

mkVoteUrl :: String -> Number -> String
mkVoteUrl v i = "/" ++ (show i) ++ "/" ++ v

voteSnippet :: forall eff. String -> Number -> ContT Unit (Eff eff) Unit
voteSnippet vote id = do
    put $ mkVoteUrl vote id
    return unit

voteSnippetSucks :: forall eff. Number -> ContT Unit (Eff eff) Unit
voteSnippetSucks = voteSnippet "sucks"

voteSnippetRocks :: forall eff. Number -> ContT Unit (Eff eff) Unit
voteSnippetRocks = voteSnippet "rocks"
