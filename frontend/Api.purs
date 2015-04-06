module Api
    ( loadSnippet
    , loadSnippets
    , voteSnippetSucks
    , voteSnippetRocks
    ) where

import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Trans
import Control.Plus
import Control.MonadPlus
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Network
import Types

loadSnippet :: forall eff. Number -> MaybeT (ContT Unit (Eff eff)) Snippet
loadSnippet id = do
    response <- lift <<< get' $ "/snippet/" ++ (show id)
    guard $ response.status == 200
    either (const empty) return $ readJSON response.text :: F Snippet

loadSnippets :: forall eff. ContT Unit (Eff eff) [Snippet]
loadSnippets = do
    response <- get' "/snippets"
    return $ either (const []) id $ readJSON response.text :: F [Snippet]

mkVoteUrl :: String -> Number -> String
mkVoteUrl v i = "/snippet/" ++ (show i) ++ "/" ++ v

voteSnippet :: forall eff. String -> Number -> ContT Unit (Eff eff) Unit
voteSnippet vote id = do
    put' $ mkVoteUrl vote id
    return unit

voteSnippetSucks :: forall eff. Number -> ContT Unit (Eff eff) Unit
voteSnippetSucks = voteSnippet "sucks"

voteSnippetRocks :: forall eff. Number -> ContT Unit (Eff eff) Unit
voteSnippetRocks = voteSnippet "rocks"
