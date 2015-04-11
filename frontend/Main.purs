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

import Control.Monad.Eff

setState :: forall eff. Request eff State -> T.Action eff State Unit
setState = T.asyncSetState <<< runContT <<< fromMaybeT error

openSnippet :: forall eff. Request eff Number -> Request eff State
openSnippet m = oneSnippet <$> (m >>= loadSnippet)

submitVote :: forall eff. (Number -> Request eff Unit) -> Snippet -> Request eff Number
submitVote vote (Snippet snippet) = (vote snippet.id) `seq` (return snippet.id)

performAction :: forall eff. Action -> Request eff State
performAction LoadAllSnippets            = allSnippets <$> loadSnippets
performAction (LoadSnippet i)            = openSnippet $ return i
performAction (VoteSnippetRocks snippet) = openSnippet $ submitVote voteSnippetRocks snippet
performAction (VoteSnippetSucks snippet) = openSnippet $ submitVote voteSnippetSucks snippet
performAction (PostNewSnippet snippet)   = openSnippet $ postNewSnippet snippet

performAction' _ (EditNewSnippet s) = T.setState $ creatingSnippet s
performAction' _ OpenNewSnippet = T.setState $ creatingSnippet emptyNewSnippet
performAction' _ a = setState <<< performAction $ a

spec :: T.Spec _ State _ Action
spec = T.simpleSpec loading performAction' render
            # T.componentWillMount LoadAllSnippets

main = do
    let component = T.createClass spec
    T.render component unit
