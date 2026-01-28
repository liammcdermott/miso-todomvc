----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingStrategies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Monad.State
import           Data.Bool
import           GHC.Generics
----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import           Miso.JSON
import           Miso.Html.Property hiding (label_)
import qualified Miso.Lens as Lens
import qualified Miso.String as S
import qualified Miso.CSS as CSS
----------------------------------------------------------------------------
default (MisoString)
----------------------------------------------------------------------------
#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
data Model
  = Model
  { inlineOpen :: Bool
  , inlineTodo :: TodoModel
  , componentOpen :: Bool
  , componentTodo :: TodoModel
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)
----------------------------------------------------------------------------
data ComponentModel
  = ComponentModel
  { componentOpenChild :: Bool
  , componentTodoChild :: TodoModel
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)
----------------------------------------------------------------------------
data TodoModel
  = TodoModel
  { entries :: [Entry]
  , field :: MisoString
  , uid :: Int
  , visibility :: MisoString
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)
----------------------------------------------------------------------------
data Entry
  = Entry
  { description :: MisoString
  , completed :: Bool
  , editing :: Bool
  , eid :: Int
  , focussed :: Bool
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel
  = Model
  { inlineOpen = False
  , inlineTodo = emptyTodoModel
  , componentOpen = False
  , componentTodo = emptyTodoModel
  }
----------------------------------------------------------------------------
emptyComponentModel :: ComponentModel
emptyComponentModel
  = ComponentModel
  { componentOpenChild = False
  , componentTodoChild = emptyTodoModel
  }
----------------------------------------------------------------------------
emptyTodoModel :: TodoModel
emptyTodoModel
  = TodoModel
  { entries = []
  , visibility = "All"
  , field = mempty
  , uid = 0
  }
----------------------------------------------------------------------------
newEntry :: MisoString -> Int -> Entry
newEntry desc eid
  = Entry
  { description = desc
  , completed = False
  , editing = False
  , eid = eid
  , focussed = False
  }
----------------------------------------------------------------------------
data Msg
  = ToggleInline
  | InlineTodo TodoMsg
  deriving (Show)
----------------------------------------------------------------------------
data ComponentMsg
  = ToggleComponent
  | ComponentTodo TodoMsg
  deriving (Show)
----------------------------------------------------------------------------
data TodoMsg
  = NoOp
  | UpdateField MisoString
  | EditingEntry Int Bool
  | UpdateEntry Int MisoString
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility MisoString
  deriving (Show)
----------------------------------------------------------------------------
main :: IO ()
main = startApp (defaultEvents <> keyboardEvents) app
----------------------------------------------------------------------------
app :: App Model Msg
app = (component emptyModel updateModel viewModel)
#ifdef VANILLA
  -- dmj: when using vanilla GHC append the styles to <head> in dev mode
  { styles =
      [ Href "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css"
      , Href "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css"
      ]
  }
#else
  { styles = [] }
#endif
----------------------------------------------------------------------------
componentExample :: Component Model ComponentModel ComponentMsg
componentExample =
  (component emptyComponentModel updateComponent viewComponentExample)
    { bindings =
        [ componentOpenLens <--> componentModelOpenLens
        , componentTodoLens <--> componentModelTodoLens
        ]
    }
----------------------------------------------------------------------------
updateModel :: Msg -> Transition Model Msg
updateModel = \case
  ToggleInline ->
    modify $ \m -> m { inlineOpen = not (inlineOpen m) }
  InlineTodo msg ->
    modify $ \m -> m { inlineTodo = updateTodo msg (inlineTodo m) }
----------------------------------------------------------------------------
updateComponent :: ComponentMsg -> Effect Model ComponentModel ComponentMsg
updateComponent = \case
  ToggleComponent ->
    modify $ \m -> m { componentOpenChild = not (componentOpenChild m) }
  ComponentTodo msg ->
    modify $ \m -> m { componentTodoChild = updateTodo msg (componentTodoChild m) }
----------------------------------------------------------------------------
updateTodo :: TodoMsg -> TodoModel -> TodoModel
updateTodo msg model@TodoModel{..} =
  case msg of
    NoOp ->
      model
    Add ->
      model
        { uid = uid + 1
        , field = mempty
        , entries = entries <> [newEntry field uid | not $ S.null field]
        }
    UpdateField str ->
      model { field = str }
    EditingEntry id' isEditing ->
      model
        { entries =
            filterMap entries (\t -> eid t == id') $ \t ->
              t { editing = isEditing
                , focussed = isEditing
                }
        }
    UpdateEntry id' task ->
      model
        { entries =
            filterMap entries ((== id') . eid) $ \t ->
              t { description = task }
        }
    Delete id' ->
      model
        { entries = filter (\t -> eid t /= id') entries
        }
    DeleteComplete ->
      model
        { entries = filter (not . completed) entries
        }
    Check id' isCompleted ->
      model
        { entries =
            filterMap entries (\t -> eid t == id') $ \t ->
              t { completed = isCompleted }
        }
    CheckAll isCompleted ->
      model
        { entries =
            filterMap entries (const True) $ \t ->
              t { completed = isCompleted }
        }
    ChangeVisibility v ->
      model { visibility = v }
----------------------------------------------------------------------------
filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs p f = [ if p x then f x else x | x <- xs ]
----------------------------------------------------------------------------
viewModel :: Model -> View Model Msg
viewModel model =
    div_
        [ class_ "todomvc-wrapper"
        ]
        [ section_
            [ class_ "examples"
            ]
            [ h1_ [] [ text "Miso component vs inline" ]
            , section_
                [ class_ "example-block"
                ]
                [ h2_ [] [ text "Component example" ]
                , "component-example" +> componentExample
                ]
            , section_
                [ class_ "example-block"
                ]
                [ h2_ [] [ text "Inline example" ]
                , inlineExampleView model
                ]
            ]
        , infoFooter
        ]
----------------------------------------------------------------------------
viewComponentExample :: ComponentModel -> View ComponentModel ComponentMsg
viewComponentExample ComponentModel{..} =
    ul_
        [ class_ "example-list"
        ]
        $ [ componentToggleItem ]
        <> componentChildren
  where
    componentToggleItem =
      li_ []
        [ button_
            [ class_ "example-button"
            , onClick ToggleComponent
            ]
            [ text "Open contrived example - component" ]
        ]
    componentChildren =
      case componentOpenChild of
        False -> []
        True ->
          [ li_ []
              [ fmap ComponentTodo (viewTodoApp "component" componentTodoChild) ]
          ]
----------------------------------------------------------------------------
inlineExampleView :: Model -> View Model Msg
inlineExampleView Model{..} =
    ul_
        [ class_ "example-list"
        ]
        $ [ inlineToggleItem ]
        <> inlineChildren
  where
    inlineToggleItem =
      li_ []
        [ button_
            [ class_ "example-button"
            , onClick ToggleInline
            ]
            [ text "Open contrived example - inline" ]
        ]
    inlineChildren =
      case inlineOpen of
        False -> []
        True ->
          [ li_ []
              [ fmap InlineTodo (viewTodoApp "inline" inlineTodo) ]
          ]
----------------------------------------------------------------------------
viewTodoApp :: MisoString -> TodoModel -> View model TodoMsg
viewTodoApp prefix TodoModel{..} =
    section_
        [ class_ "todoapp"
        ]
        [ viewInput prefix field
        , viewEntries prefix visibility entries
        , viewControls visibility entries
        ]
----------------------------------------------------------------------------
viewEntries :: MisoString -> MisoString -> [Entry] -> View model TodoMsg
viewEntries prefix visibility entries =
    section_
        [ class_ "main"
        , CSS.style_ [ CSS.visibility cssVisibility ]
        ]
        [ input_
            [ class_ "toggle-all"
            , type_ "checkbox"
            , name_ "toggle"
            , id_ (todoId prefix "toggle-all")
            , checked_ allCompleted
            , onClick $ CheckAll (not allCompleted)
            ]
        , label_
            [ for_ (todoId prefix "toggle-all") ]
            [ text $ S.pack "Mark all as complete" ]
        , ul_ [ class_ "todo-list" ] $
            flip map (filter isVisible entries) $ \t ->
                viewKeyedEntry prefix t
        ]
  where
    cssVisibility = bool "visible" "hidden" (null entries)
    allCompleted = all completed entries
    isVisible Entry{..} =
        case visibility of
            "Completed" -> completed
            "Active" -> not completed
            _ -> True
----------------------------------------------------------------------------
viewKeyedEntry :: MisoString -> Entry -> View model TodoMsg
viewKeyedEntry = viewEntry
----------------------------------------------------------------------------
viewEntry :: MisoString -> Entry -> View model TodoMsg
viewEntry prefix Entry{..} =
    li_
        [ class_ $
            S.intercalate " " $
                ["completed" | completed] <> ["editing" | editing]
        , key_ eid
        ]
        [ div_
            [ class_ "view" ]
            [ input_
                [ class_ "toggle"
                , type_ "checkbox"
                , checked_ completed
                , onClick $ Check eid (not completed)
                ]
            , label_
                [ onDoubleClick (EditingEntry eid True) ]
                [ text description ]
            , button_
                [ class_ "destroy"
                , onClick $ Delete eid
                ]
                []
            ]
        , input_
            [ class_ "edit"
            , value_ description
            , name_ "title"
            , id_ (todoId prefix ("todo-" <> S.ms eid))
            , onInput (UpdateEntry eid)
            , onBlur (EditingEntry eid False)
            , onEnter NoOp (EditingEntry eid False)
            ]
        ]
----------------------------------------------------------------------------
viewControls :: MisoString -> [Entry] -> View model TodoMsg
viewControls visibility entries =
    footer_
        [ class_ "footer"
        , hidden_ (null entries)
        ]
        [ viewControlsCount entriesLeft
        , viewControlsFilters visibility
        , viewControlsClear entriesCompleted
        ]
  where
    entriesCompleted = length . filter completed $ entries
    entriesLeft = length entries - entriesCompleted
----------------------------------------------------------------------------
viewControlsCount :: Int -> View model TodoMsg
viewControlsCount entriesLeft =
    span_
        [ class_ "todo-count" ]
        [ strong_ [] [ text $ S.ms entriesLeft ]
        , text (item_ <> " left")
        ]
  where
    item_ = S.pack $ bool " items" " item" (entriesLeft == 1)
----------------------------------------------------------------------------
viewControlsFilters :: MisoString -> View model TodoMsg
viewControlsFilters visibility =
    ul_
        [ class_ "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]
----------------------------------------------------------------------------
visibilitySwap :: MisoString -> MisoString -> MisoString -> View model TodoMsg
visibilitySwap uri visibility actualVisibility =
    li_
        []
        [ a_
            [ href_ uri
            , class_ $ S.concat ["selected" | visibility == actualVisibility]
            , onClick (ChangeVisibility visibility)
            ]
            [ text visibility ]
        ]
----------------------------------------------------------------------------
viewControlsClear :: Int -> View model TodoMsg
viewControlsClear entriesCompleted =
    button_
        [ class_ "clear-completed"
        , prop "hidden" (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text $ "Clear completed (" <> S.ms entriesCompleted <> ")" ]
----------------------------------------------------------------------------
viewInput :: MisoString -> MisoString -> View model TodoMsg
viewInput prefix task =
    header_
        [ class_ "header" ]
        [ h1_ [] [ text "todos" ]
        , input_
            [ class_ "new-todo"
            , id_ (todoId prefix "input-box")
            , placeholder_ "What needs to be done?"
            , autofocus_ True
            , value_ task
            , name_ "newTodo"
            , onInput UpdateField
            , onEnter NoOp Add
            ]
        ]
----------------------------------------------------------------------------
todoId :: MisoString -> MisoString -> MisoString
todoId prefix suffix = prefix <> "-" <> suffix
----------------------------------------------------------------------------
componentOpenLens :: Lens.Lens Model Bool
componentOpenLens =
  Lens.lens componentOpen $ \model value ->
    model { componentOpen = value }
----------------------------------------------------------------------------
componentTodoLens :: Lens.Lens Model TodoModel
componentTodoLens =
  Lens.lens componentTodo $ \model value ->
    model { componentTodo = value }
----------------------------------------------------------------------------
componentModelOpenLens :: Lens.Lens ComponentModel Bool
componentModelOpenLens =
  Lens.lens componentOpenChild $ \model value ->
    model { componentOpenChild = value }
----------------------------------------------------------------------------
componentModelTodoLens :: Lens.Lens ComponentModel TodoModel
componentModelTodoLens =
  Lens.lens componentTodoChild $ \model value ->
    model { componentTodoChild = value }
----------------------------------------------------------------------------
infoFooter :: View model Msg
infoFooter =
    footer_
        [ class_ "info" ]
        [ p_ [] [ text "Double-click to edit a todo" ]
        , p_
            []
            [ text "Written by "
            , a_ [ href_ "https://github.com/dmjio" ] [ text "@dmjio" ]
            ]
        , p_
            []
            [ text "Part of "
            , a_ [ href_ "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
----------------------------------------------------------------------------
