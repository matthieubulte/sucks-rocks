module Views (render) where

import qualified Thermite as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A

import Types

render :: T.Render _ State _ Action
render ctx (State state) _ _ = T.div' [rendered]
  where
      rendered = case state of
                      Loading                   -> loadingView
                      Error                     -> renderError ctx
                      (OneSnippet snippet)      -> renderOneSnippet ctx snippet
                      (AllSnippets snippets)    -> renderAllSnippets ctx snippets
                      (CreatingSnippet snippet) -> renderNewSnippet ctx snippet

-- State : Loading
loadingView :: T.Html _
loadingView = T.text "Loading ..."

-- State : Error
renderError :: T.Context State Action -> T.Html _
renderError ctx = T.div' [ message, T.br' [], backButton ctx ]
    where
        message = T.text $ "An unexpected error occured"

-- State : New Snippet
handleChangeTitle :: NewSnippetRecord -> T.FormEvent -> Action
handleChangeTitle snippet e = EditNewSnippet $ NewSnippet { title: getValue e, code: snippet.code }

handleChangeCode :: NewSnippetRecord -> T.FormEvent -> Action
handleChangeCode snippet e = EditNewSnippet $ NewSnippet { title: snippet.title, code: getValue e }

renderNewSnippet :: T.Context State Action -> NewSnippet -> T.Html _
renderNewSnippet ctx ns@(NewSnippet snippet) = T.div' [ editor, sendButton, backButton ctx ]
    where
        sendButton = T.button (T.onClick ctx \_ -> PostNewSnippet ns) [ T.text "Post"]
        editor = T.div' [ T.input (A.placeholder "Title"
                                   <> A.value snippet.title
                                   <> T.onChange ctx (handleChangeTitle snippet))
                                  []
                        , T.br' []
                        , T.textarea (A.placeholder "Code"
                                      <> A.value snippet.code
                                      <> T.onChange ctx (handleChangeCode snippet))
                                     []
                        ]


-- State : One Snippet
peopleThinkIt :: Number -> String -> T.Html _
peopleThinkIt n verb = T.text $ (show n) ++ " people think it " ++ verb ++ "!"

renderOneSnippet :: T.Context State Action -> Snippet -> T.Html _
renderOneSnippet ctx (Snippet snippet) = T.div' [ title, code, information, controls]
  where
      title       = T.h3' [ T.text snippet.title ]
      code        = T.pre' [ T.text snippet.code ]

      information = T.div' [ snippet.sucks `peopleThinkIt` "sucks", T.br' []
                           , snippet.rocks `peopleThinkIt` "rocks"
                           ]
      controls    = T.div' [ T.button (T.onClick ctx \_ -> VoteSnippetSucks $ Snippet snippet) [T.text "It Sucks!"]
                           , T.button (T.onClick ctx \_ -> VoteSnippetRocks $ Snippet snippet) [T.text "It Rocks!"]
                           , T.br' []
                           , backButton ctx
                           ]

-- State : All Snippets
opinionRatio :: SnippetRecord -> String
opinionRatio snippet = (show snippet.rocks) ++ "/" ++ (show $ snippet.rocks + snippet.sucks)

renderAllSnippets :: T.Context State Action -> [Snippet] -> T.Html _
renderAllSnippets ctx snippets = T.div' [ T.div' $ showSnippet <$> snippets
                                            , T.button (T.onClick ctx \_ -> OpenNewSnippet) [ T.text "New Snippet"]
                                            ]
    where
        showSnippet :: Snippet -> T.Html _
        showSnippet (Snippet snippet) = T.div' [ T.text $ snippet.title ++ " - "
                                               , T.text (opinionRatio snippet)
                                               , T.button (T.onClick ctx \_ -> LoadSnippet snippet.id) [ T.text "Open" ]
                                               ]

-- Back button
backButton :: T.Context State Action -> T.Html _
backButton ctx = T.button (T.onClick ctx \_ -> LoadAllSnippets) [T.text "Back"]


foreign import getValue
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String
