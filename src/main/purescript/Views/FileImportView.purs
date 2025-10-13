module Views.FileImportView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a', br', div, input, label, span, text)
import Concur.React.Props (ReactProps)
import Concur.React.Props as Props
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((>>>))
import Data.Array (foldl, head)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import React.SyntheticEvent (NativeEventTarget, SyntheticEvent_)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File (File)
import Web.File.FileList (FileList, items)

data DragFileEvents a = DragEnter a | DragLeave a | Drop a | File (Maybe File)

data AcceptedFileFormat = HTML

derive instance eqAcceptedFileFormat :: Eq AcceptedFileFormat

getStringRepresentation :: AcceptedFileFormat -> String
getStringRepresentation HTML = ".html"

data AcceptedFileFormats = Any | Formats (Array AcceptedFileFormat)

derive instance eqAcceptedFileFormats :: Eq AcceptedFileFormats

dragAndDropFileInputWidget :: String -> AcceptedFileFormats -> Widget HTML (Maybe File)
dragAndDropFileInputWidget fileDescription acceptedFormats = do
  dropDiv false

  where 
    acceptProps :: forall a. AcceptedFileFormats -> ReactProps a
    acceptProps  Any               = Props.emptyProp
    acceptProps (Formats formats)  = Props.accept $ foldl (\acc format -> acc <> ", " <> getStringRepresentation format) "" formats

    dropDiv :: Boolean -> Widget HTML (Maybe File)
    dropDiv highlight = do
      res <- div  [ Props.classList (Just <$> (["dropArea"] <> if highlight then ["highlight"] else []))
                  , Props._id "import"
                  , Props.onDragEnter   <#> DragEnter 
                  , Props.onDragLeave   <#> DragLeave 
                  , Props.onDropCapture <#> Drop      
                  ]
                  [ span [] [text $ "Drag your " <> fileDescription <> " here"], br'
                  , span [] [text "or"], br'
                  , label [Props.className "importButton"] [
                      span [Props.className "label"] [a' [text "select it manually"]]
                    , input [
                        Props._type "file"
                      , Props.onChange
                      , acceptProps acceptedFormats
                      ] >>= fromSyntheticEvent
                    ] <#> (items >>> head >>> File)
                  ]
      case res of
        DragEnter _    -> dropDiv true
        DragLeave _    -> dropDiv false
        Drop      a    -> (getFileFromDrop a) <#> (items >>> head)
        File      file -> pure file

    fromSyntheticEvent :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Widget HTML FileList
    fromSyntheticEvent  se = pure $ (unsafeCoerce se).target.files

    getFileFromDrop :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Widget HTML FileList
    getFileFromDrop se = pure $ (unsafeCoerce se).dataTransfer.files

