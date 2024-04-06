module Views.UserAreaView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, header, li, li', span, text, ul)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>))
import Control.Bind (bind)
import Control.Category ((>>>))
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Duration (Days)
import Data.Tuple (Tuple(..), swap)
import DataModel.Credentials (Credentials)
import DataModel.UserVersions.User (UserPreferences, DonationInfo)
import DataModel.WidgetState (UserAreaPage(..), UserAreaState, UserAreaSubmenu(..), ImportState)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.EnvironmentalVariables (currentCommit)
import Functions.Events (keyboardShortcut)
import Functions.State (isOffline)
import Views.ChangePasswordView (changePasswordView)
import Views.Components (Enabled(..), footerComponent)
import Views.DeleteUserView (deleteUserView)
import Views.DonationViews (donationUserArea)
import Views.DonationViews as DonationEvent
import Views.ExportView (ExportEvent, exportView)
import Views.ImportView (importView, initialImportState)
import Views.SetPinView (PinEvent, setPinView)
import Views.UserPreferencesView (userPreferencesView)

data UserAreaEvent    = CloseUserAreaEvent
                      | ChangeUserAreaSubmenu (Map UserAreaSubmenu Boolean)
                      | OpenUserAreaPage UserAreaPage
                      | UpdateUserPreferencesEvent UserPreferences
                      | ChangePasswordEvent String
                      | SetPinEvent PinEvent
                      | DeleteAccountEvent
                      | ImportCardsEvent ImportState
                      | ExportEvent ExportEvent
                      | UpdateDonationLevel Days
                      | LockEvent
                      | LogoutEvent

userAreaInitialState :: UserAreaState
userAreaInitialState = { showUserArea: false, userAreaOpenPage: None, importState: initialImportState, userAreaSubmenus: fromFoldable [(Tuple Account false), (Tuple Data false)]}

userAreaView :: UserAreaState -> UserPreferences -> Credentials -> Maybe DonationInfo -> Int -> Boolean -> Widget HTML (Tuple UserAreaEvent UserAreaState)
userAreaView state@{showUserArea, userAreaOpenPage, importState, userAreaSubmenus} userPreferences credentials donationInfo numberOfCards pinExists = do
  commitHash <- liftEffect currentCommit
  ((div [Props._id "userPage", Props.className (if showUserArea then "open" else "closed")] [
      div [Props.onClick, Props.className "mask"] [] $> CloseUserAreaEvent
    , div [Props.className "panel"] [
        header [] [div [] [button [Props.onClick] [text "menu"]]] $> CloseUserAreaEvent
      , userAreaMenu
      , footerComponent commitHash
      ]
    , userAreaInternalView
    ])
    <> 
    ((keyboardShortcut ["l o c k"] # liftAff) $> LockEvent)
  ) <#> (Tuple state >>> swap)

  where
    userAreaMenu :: Widget HTML UserAreaEvent
    userAreaMenu = do
      offline    <- liftEffect isOffline
      ul [Props._id "userSidebar"] [
        subMenu Account "Account" [
          subMenuElement Preferences    (Enabled $ not offline) "Preferences"
        , subMenuElement ChangePassword (Enabled $ not offline) "Passphrase"
        , subMenuElement Pin            (Enabled   true)        "Device PIN"
        , subMenuElement Delete         (Enabled $ not offline) "Delete account"
        ]
      , subMenu Data    "Data"    [
          subMenuElement Import         (Enabled $ not offline) "Import"
        , subMenuElement Export         (Enabled $ not offline) "Export"
        ]
      , subMenuElement   Donate         (Enabled   true)        "Donate" <#> OpenUserAreaPage
      , li' [a      [Props.className "link", Props.href "/about/app", Props.target "_blank"] [span [] [text "About"]]]
      , li' [button [Props.onClick, Props._id "lockButton"]                                  [span [] [text "Lock"]]]   $> LockEvent
      , li' [button [Props.onClick]                                                          [span [] [text "Logout"]]] $> LogoutEvent
      ]

      where
        subMenu :: UserAreaSubmenu -> String -> Array (Widget HTML UserAreaPage) -> Widget HTML UserAreaEvent
        subMenu userAreaSubmenu label subMenuElements = li [] [
          button [Props.onClick] [span [] [text label]] $> ChangeUserAreaSubmenu (invertSubmenuValue userAreaSubmenu)
        , ul [Props.classList [Just "userSidebarSubitems", if isSubmenuOpen userAreaSubmenu then Nothing else Just "hidden"]]
            subMenuElements <#> OpenUserAreaPage
        ]

        subMenuElement :: UserAreaPage -> Enabled -> String -> Widget HTML UserAreaPage
        subMenuElement userAreaPage (Enabled enabled) label = li [Props.classList [if userAreaOpenPage == userAreaPage then Just "selected" else Nothing, Just "subMenuElement"]] [
          button [Props.onClick, Props.disabled (not enabled)] [span [] [text label]] $> (if userAreaOpenPage == userAreaPage then None else userAreaPage)
        ]
        
        invertSubmenuValue :: UserAreaSubmenu -> Map UserAreaSubmenu Boolean
        invertSubmenuValue userAreaSubmenu = insert userAreaSubmenu (not $ isSubmenuOpen userAreaSubmenu) userAreaSubmenus

        isSubmenuOpen :: UserAreaSubmenu -> Boolean
        isSubmenuOpen userAreaSubmenu = fromMaybe false $ lookup userAreaSubmenu userAreaSubmenus

    userAreaInternalView :: Widget HTML UserAreaEvent
    userAreaInternalView = 
      case userAreaOpenPage of
        Preferences     -> frame (userPreferencesView userPreferences         <#> UpdateUserPreferencesEvent)
        ChangePassword  -> frame (changePasswordView  credentials             <#> ChangePasswordEvent)
        Pin             -> frame (setPinView          pinExists               <#> SetPinEvent)
        Delete          -> frame (deleteUserView      credentials              $> DeleteAccountEvent)
        Import          -> frame (importView          importState             <#> ImportCardsEvent)
        Export          -> frame (exportView                                  <#> ExportEvent)
        Donate          -> frame (donationUserArea numberOfCards donationInfo <#> (\res -> case res of
                                                                                  DonationEvent.CloseDonationPage     -> OpenUserAreaPage None
                                                                                  DonationEvent.UpdateDonationLevel m -> UpdateDonationLevel m
                                                                                  ))
        About           -> frame (text "This is Clipperz")
        None            -> emptyUserComponent

      where
        frame :: Widget HTML UserAreaEvent -> Widget HTML UserAreaEvent
        frame c = div [Props.classList $ Just <$> ["extraFeatureContent", "open"]] [
          header [] [div [] [button [Props.onClick] [text "close"]]] $> OpenUserAreaPage None
        , c
        ]

        emptyUserComponent :: forall a. Widget HTML a
        emptyUserComponent = (div [Props.className "extraFeatureContent"] [])
