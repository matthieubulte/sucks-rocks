module Api
    ( loadSnippet
    , loadSnippets
    , voteSnippetSucks
    , voteSnippetRocks
    , postNewSnippet
    ) where

import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Control.Plus
import Control.MonadPlus
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Network
import Types
import Utils

loadSnippet :: forall eff. Number -> Request eff Snippet
loadSnippet id = do
    response <- lift <<< get' $ "/snippet/" ++ (show id)
    guard $ response.status == 200
    either (const empty) return $ readJSON response.text :: F Snippet

loadSnippets :: forall eff. Request eff [Snippet]
loadSnippets = do
    response <- lift $ get' "/snippets"
    return $ either (const []) id $ readJSON response.text :: F [Snippet]

mkVoteUrl :: String -> Number -> String
mkVoteUrl v i = "/snippet/" ++ (show i) ++ "/" ++ v

voteSnippet :: forall eff. String -> Number -> Request eff Unit
voteSnippet vote id = do
    lift <<< put' $ mkVoteUrl vote id
    return unit

voteSnippetSucks :: forall eff. Number -> Request eff Unit
voteSnippetSucks = voteSnippet "sucks"

voteSnippetRocks :: forall eff. Number -> Request eff Unit
voteSnippetRocks = voteSnippet "rocks"

postNewSnippet :: forall eff. NewSnippet -> Request eff Number
postNewSnippet (NewSnippet newSnippet) = do
    response <- lift $ post "/snippet" [] (stringify newSnippet)
    guard $ response.status == 200
    either (const empty) return $ readJSON response.text :: F Number
