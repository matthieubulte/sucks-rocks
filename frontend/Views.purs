module Views (render) where

import qualified Thermite as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A

import Types

render :: T.Render State Unit Action
render context (State state) props = T.div' [rendered]
  where
      rendered = case state of
                      Loading                -> loadingView
                      Error                  -> errorView
                      (OneSnippet snippet)   -> renderOneSnippet context snippet props
                      (AllSnippets snippets) -> renderAllSnippets context snippets props

-- State : Loading
loadingView :: T.Html _
loadingView = T.text "Loading ..."

-- State : Error
errorView :: T.Html _
errorView = T.text $ "An unexpected error occured"

-- State : One Snippet
peopleThinkIt :: Number -> String -> T.Html _
peopleThinkIt n verb = T.text $ (show n) ++ " people think it " ++ verb ++ "!"

renderOneSnippet :: forall props. T.Context State props Action -> Snippet -> props -> T.Html Action
renderOneSnippet context (Snippet snippet) props = T.div' [ code, information, controls]
  where
      code        = T.pre' [ T.text snippet.code ]

      information = T.div' [ snippet.sucks `peopleThinkIt` "sucks", T.br' []
                           , snippet.rocks `peopleThinkIt` "rocks"
                           ]
      controls    = T.div' [ T.button [ T.onClick context \_ -> VoteSnippetSucks $ Snippet snippet] [T.text "It Sucks!"]
                           , T.button [ T.onClick context \_ -> VoteSnippetRocks $ Snippet snippet] [T.text "It Rocks!"]
                           , T.br' []
                           , T.button [ T.onClick context \_ -> LoadAllSnippets] [T.text "Back"]
                           ]

-- State : All Snippets
opinionRatio :: SnippetRecord -> String
opinionRatio snippet = (show snippet.rocks) ++ "/" ++ (show $ snippet.rocks + snippet.sucks)

renderAllSnippets :: forall props. T.Context State props Action -> [Snippet] -> props -> T.Html Action
renderAllSnippets context snippets props = T.div' $ showSnippet <$> snippets
    where
        showSnippet :: Snippet -> T.Html _
        showSnippet (Snippet snippet) = T.div' [ T.text $ snippet.title ++ " - "
                                               , T.text (opinionRatio snippet)
                                               , T.button [ T.onClick context \_ -> LoadSnippet snippet.id ] [ T.text "Open" ]
                                               ]

