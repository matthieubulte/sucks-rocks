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
                      Loading                   -> loadingView
                      Error                     -> renderError context
                      (OneSnippet snippet)      -> renderOneSnippet context snippet props
                      (AllSnippets snippets)    -> renderAllSnippets context snippets props
                      (CreatingSnippet snippet) -> renderNewSnippet context snippet props

-- State : Loading
loadingView :: T.Html _
loadingView = T.text "Loading ..."

-- State : Error
renderError :: forall props. T.Context State props Action -> T.Html Action
renderError context = T.div' [ message, T.br' [], backButton context ]
    where
        message = T.text $ "An unexpected error occured"

-- State : New Snippet
handleChangeTitle :: NewSnippetRecord -> T.FormEvent -> Action
handleChangeTitle snippet e = EditNewSnippet $ NewSnippet { title: getValue e, code: snippet.code }

handleChangeCode :: NewSnippetRecord -> T.FormEvent -> Action
handleChangeCode snippet e = EditNewSnippet $ NewSnippet { title: snippet.title, code: getValue e }

renderNewSnippet :: forall props. T.Context State props Action -> NewSnippet -> props -> T.Html Action
renderNewSnippet context ns@(NewSnippet snippet) props = T.div' [ editor, sendButton, backButton context ]
    where
        sendButton = T.button [ T.onClick context \_ -> PostNewSnippet ns ] [ T.text "Post"]
        editor = T.div' [ T.input [ A.placeholder "Title"
                                  , A.value snippet.title
                                  , T.onChange context $ handleChangeTitle snippet
                                  ] []
                        , T.br' []
                        , T.textarea [ A.placeholder "Code"
                                     , A.value snippet.code
                                     , T.onChange context $ handleChangeCode snippet
                                     ] []
                        ]


-- State : One Snippet
peopleThinkIt :: Number -> String -> T.Html _
peopleThinkIt n verb = T.text $ (show n) ++ " people think it " ++ verb ++ "!"

renderOneSnippet :: forall props. T.Context State props Action -> Snippet -> props -> T.Html Action
renderOneSnippet context (Snippet snippet) props = T.div' [ title, code, information, controls]
  where
      title       = T.h3' [ T.text snippet.title ]
      code        = T.pre' [ T.text snippet.code ]

      information = T.div' [ snippet.sucks `peopleThinkIt` "sucks", T.br' []
                           , snippet.rocks `peopleThinkIt` "rocks"
                           ]
      controls    = T.div' [ T.button [ T.onClick context \_ -> VoteSnippetSucks $ Snippet snippet] [T.text "It Sucks!"]
                           , T.button [ T.onClick context \_ -> VoteSnippetRocks $ Snippet snippet] [T.text "It Rocks!"]
                           , T.br' []
                           , backButton context
                           ]

-- State : All Snippets
opinionRatio :: SnippetRecord -> String
opinionRatio snippet = (show snippet.rocks) ++ "/" ++ (show $ snippet.rocks + snippet.sucks)

renderAllSnippets :: forall props. T.Context State props Action -> [Snippet] -> props -> T.Html Action
renderAllSnippets context snippets props = T.div' [ T.div' $ showSnippet <$> snippets
                                                  , T.button [ T.onClick context \_ -> OpenNewSnippet ] [ T.text "New Snippet"]
                                                  ]
    where
        showSnippet :: Snippet -> T.Html _
        showSnippet (Snippet snippet) = T.div' [ T.text $ snippet.title ++ " - "
                                               , T.text (opinionRatio snippet)
                                               , T.button [ T.onClick context \_ -> LoadSnippet snippet.id ] [ T.text "Open" ]
                                               ]

-- Back button
backButton :: forall props. T.Context State props Action -> T.Html Action
backButton context = T.button [ T.onClick context \_ -> LoadAllSnippets] [T.text "Back"]


foreign import getValue
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String
