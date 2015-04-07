module Main (main) where

import Network
import Types
import Api
import Views
import Utils

import Control.Monad.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Cont.Trans

import qualified Thermite as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

setState = T.asyncSetState <<< runContT
maybeSetState = setState <<< fromMaybeT error

openSnippet i = oneSnippet <$> loadSnippet i

openSnippet' m = maybeSetState $ oneSnippet <$> (m >>= loadSnippet)

submitVote :: forall eff. _ -> Snippet -> RequestStack eff Number
submitVote vote (Snippet snippet) = do
    lift $ vote snippet.id
    return snippet.id

postAndReload newSnippet = do
    createdSnippetId <- postNewSnippet newSnippet
    openSnippet createdSnippetId

performAction :: forall eff. T.PerformAction Unit Action (T.Action eff State)
performAction _ LoadAllSnippets            = setState $ allSnippets <$> loadSnippets
performAction _ (LoadSnippet id)           = openSnippet' $ return id
performAction _ (VoteSnippetRocks snippet) = openSnippet' $ submitVote voteSnippetRocks snippet
performAction _ (VoteSnippetSucks snippet) = openSnippet' $ submitVote voteSnippetSucks snippet
performAction _ (PostNewSnippet snippet)   = maybeSetState $ postAndReload snippet

spec :: T.Spec _ State _ Action
spec = T.simpleSpec loading performAction render
            # T.componentWillMount LoadAllSnippets

main = do
    let component = T.createClass spec
    T.render component unit
