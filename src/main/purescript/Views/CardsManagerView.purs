module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire)
import Concur.React (HTML, affAction)
import Concur.React.DOM (button, dd, div, dl, dt, h3, h4, header, li, ol, span, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>), (<|>))
import Control.Alternative (empty, (*>))
import Control.Applicative (pure)
import Control.Bind ((=<<), (>>=))
import Control.Category ((<<<), (>>>))
import Data.Array (foldl, fromFoldable, mapWithIndex)
import Data.CommutativeRing (add)
import Data.Either (either)
import Data.Eq ((/=), (==))
import Data.Function (flip, (#), ($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.List (List, elemIndex, index, length)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (max, min)
import Data.Ring (sub, (-))
import Data.Semigroup ((<>))
import Data.Set (Set, unions)
import Data.Time.Duration (Days)
import Data.Tuple (Tuple(..), swap)
import Data.Unit (unit)
import DataModel.CardVersions.Card (Card, emptyCard)
import DataModel.IndexVersions.Index (CardEntry(..), Index(..))
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.Proxy (ProxyInfo)
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState(..))
import Effect.Class (liftEffect)
import Functions.Donations (DonationLevel(..))
import Functions.EnvironmentalVariables (donationIFrameURL)
import Functions.Events (blur, focus, keyboardShortcut, select)
import IndexFilterView (Filter(..), FilterData, FilterViewStatus(..), filteredEntries, getClassNameFromFilterStatus, indexFilterView, initialFilterData, shownEntries)
import OperationalWidgets.Sync (SyncData)
import Views.CardViews (CardEvent(..), cardView)
import Views.Components (proxyInfoComponent)
import Views.CreateCardView (CardFormData, createCardView)
import Views.DeviceSyncView (EnableSync, syncProgressBar)
import Views.DonationViews (donationIFrame)
import Views.DonationViews as DonationEvent

data CardManagerEvent = AddCardEvent        Card
                      | CloneCardEvent      CardEntry
                      | DeleteCardEvent     CardEntry
                      | EditCardEvent       (Tuple CardEntry Card)
                      | ArchiveCardEvent    CardEntry
                      | RestoreCardEvent    CardEntry
                      | OpenCardFormEvent   (Maybe (Tuple CardEntry Card))
                      | OpenUserAreaEvent
                      | ShowShortcutsEvent  Boolean
                      | ShowDonationEvent   Boolean
                      | ChangeFilterEvent   FilterData
                      | NavigateCardsEvent  NavigateCardsEvent
                      | UpdateDonationLevel Days
                      | UpdateCardForm      CardFormData

data NavigateCardsEvent = Move Int | Open (Maybe CardEntry) | Close (Maybe Int)

cardManagerInitialState :: CardManagerState
cardManagerInitialState = {
  filterData: initialFilterData
, highlightedEntry: Nothing
, cardViewState: NoCard
, showShortcutsHelp: false
, showDonationOverlay: false
}

type EnableShortcuts = Boolean

cardsManagerView :: CardManagerState -> Index -> PasswordGeneratorSettings -> DonationLevel -> ProxyInfo -> EnableShortcuts -> EnableSync -> Maybe (Wire (Widget HTML) SyncData) -> Widget HTML (Tuple CardManagerEvent CardManagerState)
cardsManagerView state@{filterData: filterData@{filterViewStatus, filter, archived, searchString}, highlightedEntry, cardViewState, showShortcutsHelp, showDonationOverlay} index'@(Index {entries}) userPasswordGeneratorSettings donationLevel proxyInfo enableShortcuts enableSync syncDataWire = do
  div [Props._id "cardsManager", Props.className $ "filterView_" <> getClassNameFromFilterStatus filterViewStatus] [
    indexFilterView filterData index' >>= updateFilterData
  , div [Props.className "cardToolbarFrame"] [
      toolbarHeader "frame"
    , syncProgressBar enableSync syncDataWire
    , proxyInfoComponent proxyInfo [Just "withDate"]
    , div [Props._id "mainView", Props.className (if cardViewState /= NoCard then "CardViewOpen" else "CardViewClose")] [
        div [Props._id "indexView"] [
          toolbarHeader "cardList"
        , div [Props.className "addCard"] [
            button [Props.onClick, Props.className "addCard" ] [span [] [text "add card"]] $> OpenCardFormEvent Nothing
          ]
        , (indexView sortedCards getHighlightedEntry) <#> (NavigateCardsEvent <<< Open <<< Just)
        , donationButton (donationLevel == DonationInfo) showDonationOverlay
        ]
      , div [Props._id "card"] [
          mainStageView cardViewState
        ]
      ]
    , donationButton (donationLevel == DonationWarning) showDonationOverlay
    ]
  ] <> (if enableShortcuts then shortcutsHandlers else empty)
    <> (shortcutsHelp     showShortcutsHelp)
  <#> (Tuple state >>> swap)

  where

    sortedCards :: List CardEntry
    sortedCards  = List.sort $ filteredEntries filter (shownEntries entries selectedEntry archived)

    selectedEntry :: Maybe CardEntry
    selectedEntry = case cardViewState of
      Card                   _ entry  -> Just entry
      CardForm _ (ModifyCard _ entry) -> Just entry
      _                               -> Nothing

    getHighlightedEntry :: Maybe Int
    getHighlightedEntry = highlightedEntry <|> (selectedEntry >>= flip elemIndex sortedCards)

    increaseIndex :: Int -> Int
    increaseIndex numberOfCards = min (numberOfCards-1) (maybe 0 (add 1)      getHighlightedEntry)
    
    decreaseIndex ::        Int
    decreaseIndex               = max  0                (maybe 0 (flip sub 1) getHighlightedEntry)

    getCardToOpen :: List CardEntry -> Maybe CardEntry
    getCardToOpen entries' = highlightedEntry >>= (index entries')

    updateFilterData :: FilterData -> Widget HTML CardManagerEvent
    updateFilterData filterData' = 
      case filterData'.filterViewStatus of
        FilterViewClosed -> blur  "searchInputField" # liftEffect
        FilterViewOpen   -> 
          case filterData'.filter of
            Search _     -> focus "searchInputField" # liftEffect
            _            -> pure unit
        $> ChangeFilterEvent filterData'

    getFilterHeader :: forall a. Filter -> Widget HTML a
    getFilterHeader f =
      case f of
        All                  -> text "clipperz"
        Recent               -> text "recent"
        Untagged             -> text "untagged"
        Search ""            -> text "clipperz"
        Search searchString' -> span [] [text searchString']
        Tag    tag           -> span [] [text tag]

    toolbarHeader :: String -> Widget HTML CardManagerEvent
    toolbarHeader className = header [Props.className className] [
      div [Props.className "tags"] [button [Props.onClick] [text "tags"]] *> updateFilterData (filterData {filterViewStatus = FilterViewOpen})
    , div [Props.className "selection"] [getFilterHeader filter]
    , OpenUserAreaEvent <$ div [Props.className "menu"] [button [Props.onClick] [text "menu"]]
    ]

    allTags :: Set String
    allTags = unions $ (\(CardEntry entry) -> entry.tags) <$> fromFoldable entries

    shortcutsHandlers :: Widget HTML CardManagerEvent
    shortcutsHandlers =
         ((keyboardShortcut ["*"                  ] # affAction) *>                                             updateFilterData initialFilterData)
      <> ((keyboardShortcut ["/"                  ] # affAction) *> (select "searchInputField" # liftEffect) *> updateFilterData filterData {filterViewStatus = FilterViewOpen, filter = Search searchString})
      <> ((keyboardShortcut ["j", "down"          ] # affAction) $> (NavigateCardsEvent $ Move (increaseIndex (length sortedCards))))
      <> ((keyboardShortcut ["k", "up"            ] # affAction) $> (NavigateCardsEvent $ Move (decreaseIndex                     )))
      <> ((keyboardShortcut ["l", "right", "enter"] # affAction) $> (NavigateCardsEvent $ Open (getCardToOpen sortedCards         )))
      <> ((keyboardShortcut ["h", "left" , "esc"  ] # affAction) $> if showShortcutsHelp
                                                               then  ShowShortcutsEvent   false
                                                               else (NavigateCardsEvent $ Close getHighlightedEntry                ))
      <> ((keyboardShortcut ["?"                  ] # affAction) $>  ShowShortcutsEvent   true                                      )

    mainStageView :: CardViewState -> Widget HTML CardManagerEvent
    mainStageView  NoCard                               = div [] []
    mainStageView (Card card cardEntry)                 = cardView proxyInfo card cardEntry <#> handleCardEvents
    mainStageView (CardForm cardFormData cardFormInput) = createCardView cardFormData inputCard allTags userPasswordGeneratorSettings proxyInfo <#> (either UpdateCardForm (maybe (NavigateCardsEvent $ viewCardStateUpdate) outputEvent))
      where

        inputCard = case cardFormInput of
          NewCard                    -> emptyCard
          NewCardFromFragment card   -> card
          ModifyCard          card _ -> card
        
        viewCardStateUpdate = case cardFormInput of
          NewCard                   -> Close  Nothing
          NewCardFromFragment _     -> Close  getHighlightedEntry 
          ModifyCard _    cardEntry -> Open  (Just cardEntry)

        outputEvent = case cardFormInput of
          NewCard                -> AddCardEvent
          NewCardFromFragment _  -> AddCardEvent
          ModifyCard _ cardEntry -> EditCardEvent <<< Tuple cardEntry

    handleCardEvents :: CardEvent -> CardManagerEvent
    handleCardEvents (Edit    cardEntry card) = OpenCardFormEvent $ Just (Tuple cardEntry card)
    handleCardEvents (Clone   cardEntry     ) = CloneCardEvent   cardEntry
    handleCardEvents (Archive cardEntry     ) = ArchiveCardEvent cardEntry
    handleCardEvents (Restore cardEntry     ) = RestoreCardEvent cardEntry
    handleCardEvents (Delete  cardEntry     ) = DeleteCardEvent  cardEntry
    handleCardEvents (Exit                  ) = NavigateCardsEvent $ Close getHighlightedEntry

-- ==================================================================                                                                                                                             

shortcutsHelp :: Boolean -> Widget HTML CardManagerEvent
shortcutsHelp showShortcutsHelp = div [Props.classList [Just "shortcutsHelp", Just "disableOverlay", hiddenClass]] [
  div [Props.className "mask", Props.onClick] []
, div [Props.className "helpBox"] [
      header [] [
        h3 [] [text "Keyboard shortcuts"]
      , button [Props.className "close", Props.onClick] [text "close"]
      ]
    , div [Props.className "helpContent"] [
        helpBlock "Search"      [ Tuple [ Tuple "/"        Nothing    ] "search cards"
                                , Tuple [ Tuple "*"        Nothing    ] "select all cards"
                                ]       
      , helpBlock "Navigation"  [ Tuple [ Tuple "h"       (Just "or")
                                        , Tuple "<left>"  (Just "or")
                                        , Tuple "<esc>"    Nothing
                                        ]                                "exit current selection"
                                , Tuple [ Tuple "l"       (Just "or")
                                        , Tuple "<right>" (Just "or")
                                        , Tuple "<enter>"  Nothing
                                        ]                                "select detail"   
                                , Tuple [ Tuple "k"        (Just "/")
                                        , Tuple "j"        (Just "or")
                                        , Tuple "<up>"     (Just "/")
                                        , Tuple "<down>"    Nothing
                                        ]                                "previous/next card"
                                ]
      , helpBlock "Misc"        [ Tuple [ Tuple "l o c k"   Nothing   ] "lock application"
                                ] 
    ]
  ]
] $> ShowShortcutsEvent false
  where
    helpBlock :: forall a. String 
                        -> Array (Tuple (Array (Tuple String (Maybe String))) String) 
                        -> Widget HTML a
    helpBlock title shortcuts = 
      div [Props.className "helpBlock"] [
        h4 [] [text title]
      , dl [] $ foldl (\shortcutsList (Tuple shortcut description) -> shortcutsList <> [
          dt [] $ foldl (\shortcutCombination (Tuple key operator) -> 
               shortcutCombination <> [ span [Props.className "key"] [text key] ] <> case operator of
                                                                                      Just op -> [span [Props.className "operator"] [text op]]
                                                                                      Nothing -> []
          ) [] shortcut
        , dd [] [text description]
        ]) [] shortcuts
      ]
    
    hiddenClass :: Maybe String
    hiddenClass = if showShortcutsHelp
                  then Nothing
                  else Just "hidden"

-- ==================================================================                                                                                                                             

donationButton :: Boolean -> Boolean -> Widget HTML CardManagerEvent
donationButton false _           = empty
donationButton true  showOverlay =
  div [Props.classList [Just "donationButton"]] ([
    button [Props.onClick] [span [] [text "Support Clipperz"]] $> ShowDonationEvent true
  ] <> (if (not showOverlay) then [] else [
    div [Props.className "disableOverlay"] [
      div [Props.className "mask", Props.onClick] [] $> ShowDonationEvent false
    , div [Props.className "dialog"] [
        (donationIFrame =<< (liftEffect $ donationIFrameURL "button/")) 
        <#> 
        (\res -> case res of
          DonationEvent.CloseDonationPage     ->  ShowDonationEvent false
          DonationEvent.UpdateDonationLevel m ->  UpdateDonationLevel m
        )
      ]
    ]
  ]))

-- ==================================================================                                                                                                                             

indexView :: List CardEntry -> Maybe Int -> Widget HTML CardEntry
indexView sortedCards selectedEntry = ol [] (
  flip mapWithIndex (fromFoldable sortedCards) (\index cardEntry@(CardEntry { title, archived: archived' }) -> 
    li [Props.classList [archivedClass archived', selectedClass index], cardEntry <$ Props.onClick] [
      text title
    ]
  )
) 

  where
    archivedClass archived' = if archived'                   then Just "archived" else Nothing
    selectedClass index     = if selectedEntry == Just index then Just "selected" else Nothing