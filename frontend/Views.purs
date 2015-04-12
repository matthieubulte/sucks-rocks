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
render ctx (State state) _ _ = page ctx content
  where
      content = case state of
                     Loading                   -> loadingView
                     Error                     -> errorView
                     (OneSnippet snippet)      -> renderOneSnippet ctx snippet
                     (AllSnippets snippets)    -> renderAllSnippets ctx snippets
                     (CreatingSnippet snippet) -> renderNewSnippet ctx snippet

-- State : Loading
loadingView :: T.Html _
loadingView = T.text "Loading ..."

-- State : Error
errorView :: T.Html _
errorView = T.text $ "An unexpected error occured"

-- State : New Snippet
handleChangeTitle :: NewSnippetRecord -> T.FormEvent -> Action
handleChangeTitle snippet e = EditNewSnippet $ NewSnippet { title: getValue e, code: snippet.code }

handleChangeCode :: NewSnippetRecord -> T.FormEvent -> Action
handleChangeCode snippet e = EditNewSnippet $ NewSnippet { title: snippet.title, code: getValue e }

renderNewSnippet :: T.Context State Action -> NewSnippet -> T.Html _
renderNewSnippet ctx ns@(NewSnippet snippet) = T.div' [ titleInput, sendButton, newline, editor ]
    where
        sendButton = T.button (T.onClick ctx \_ -> PostNewSnippet ns) [ T.text "Post"]

        titleInput = T.input (A.placeholder "Title"
                             <> A._id "title-input"
                             <> A.value snippet.title
                             <> T.onChange ctx (handleChangeTitle snippet))
                             []
        editor = T.textarea (A.placeholder "Code"
                              <> A.value snippet.code
                              <> T.onChange ctx (handleChangeCode snippet))
                             []


-- State : One Snippet
peopleThinkIt :: Number -> String -> T.Html _
peopleThinkIt n verb = T.text $ (show n) ++ " people think it " ++ verb ++ "!"

renderOneSnippet :: T.Context State Action -> Snippet -> T.Html _
renderOneSnippet ctx (Snippet snippet) = T.div' [ title, code, information, controls]
  where
      title       = T.h3'  [ T.text snippet.title ]
      code        = T.pre' [ T.text snippet.code ]

      information = T.div' [ snippet.sucks `peopleThinkIt` "sucks"
                           , newline
                           , snippet.rocks `peopleThinkIt` "rocks"
                           ]
      controls    = T.div' [ T.button (T.onClick ctx \_ -> VoteSnippetSucks $ Snippet snippet) [T.text "It Sucks!"]
                           , T.button (T.onClick ctx \_ -> VoteSnippetRocks $ Snippet snippet) [T.text "It Rocks!"]
                           ]

-- State : All Snippets
opinionRatio :: SnippetRecord -> String
opinionRatio snippet = (show snippet.rocks) ++ "/" ++ (show $ snippet.rocks + snippet.sucks)

renderAllSnippets :: T.Context State Action -> [Snippet] -> T.Html _
renderAllSnippets ctx snippets = T.div' [ table $ row <$> snippets
                                        , T.button (T.onClick ctx \_ -> OpenNewSnippet) [ T.text "New Snippet"]
                                        ]
    where
        table :: [T.Html _] -> T.Html _
        table rows = T.table' [ T.tbody' $ header : rows ]

        header :: T.Html _
        header = T.tr' [ T.th' [ T.text "Title" ]
                       , T.th' [ T.text "Rock Votes" ]
                       , T.th' [ T.text "Suck Votes" ]
                       ]

        row :: Snippet -> T.Html _
        row (Snippet s) = T.tr' [ T.td' [ T.button (T.onClick ctx (\_ -> LoadSnippet s.id)
                                                   <> A.className "simple-button blue")
                                                   [ T.text s.title ]
                                        ]
                                , T.td' [ T.text $ show s.rocks ]
                                , T.td' [ T.text $ show s.sucks ]
                                ]

-- Page Template
page :: T.Context State Action -> T.Html _ -> T.Html _
page ctx content = T.div' [ header
                          , content
                          ]
    where
        header :: T.Html _
        header = T.header' [ T.button (T.onClick ctx (\_ -> LoadAllSnippets)
                                      <> A.className "simple-button")
                                      [ T.text "# Sucks Rocks"]
                           ]

newline :: T.Html _
newline = T.br' []

foreign import getValue
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String
