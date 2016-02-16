{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, OverloadedStrings, TemplateHaskell, TupleSections,
             TypeFamilies #-}
module React.Flux.Component.SComponent.Dropdown
    ( -- * Property
      DropdownProperty (..)
    , dropdownPropertyClass
    , dropdownPropertyTabIndex
    , dropdownPropertyAnimation
    , defaultDropdownProperty
      -- * Element
    , dropdownMenu_
    , dropdownMenu_'
    , dropdownItem_
    , dropdownItem_'
    , dropdownDivider_
    , dropdownDivider_'
    , dropdownHeader_
    , dropdownHeader_'
    , dropdownButton_
    , dropdownButton_'
    , dropdownAnchor_
    , dropdownAnchor_'
    , dropdown_
      -- * Views
    , dropdown
    ) where

import           Control.Lens
import qualified Data.Text                                       as T
import           React.Flux                                      (($=), (@=))
import qualified React.Flux                                      as RF
import           React.Flux.Component.SComponent.Internal.Import (Generic, NFData, Typeable)
import           React.Flux.Component.SComponent.Internal.Util
import           React.Flux.Component.SComponent.Types

--------------------------------------------------------------------------------
-- Property
--------------------------------------------------------------------------------

data DropdownProperty = DropdownProperty
    { _dropdownPropertyClass     :: [T.Text]
    , _dropdownPropertyTabIndex  :: Int
    , _dropdownPropertyAnimation :: Bool
    } deriving (Show, Typeable, Generic, NFData)

makeLenses ''DropdownProperty

defaultDropdownProperty :: DropdownProperty
defaultDropdownProperty = DropdownProperty [] 9999 False

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data DropdownState = DropdownState
    { _stateIsOpened :: Bool
    } deriving (Show, Typeable)

makeLenses ''DropdownState

defaultDropdownState :: DropdownState
defaultDropdownState = DropdownState False

--------------------------------------------------------------------------------
-- Element
--------------------------------------------------------------------------------

dropdownMenu_ :: [ClassName] -> [RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownMenu_ = classNameElement RF.div_ ["s-dropdown-menu", "dropdown-menu"]

dropdownMenu_' :: RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownMenu_' = classNameElement' RF.div_ ["s-dropdown-menu", "dropdown-menu"]

dropdownItem_ :: [ClassName] -> [RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownItem_ = classNameElement RF.div_ ["s-dropdown-item", "dropdown-item"]

dropdownItem_' :: RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownItem_' = classNameElement' RF.div_ ["s-dropdown-item", "dropdown-item"]

dropdownDivider_ :: [ClassName] -> [RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler ()
dropdownDivider_ classes props = classNameElement RF.div_ ["s-dropdown-divider", "dropdown-divider"] classes props $ return ()

dropdownDivider_' :: RF.ReactElementM eventHandler ()
dropdownDivider_' = classNameElement' RF.div_ ["s-dropdown-divider", "dropdown-divider"] $ return ()

dropdownHeader_ :: [ClassName] -> [RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownHeader_ = classNameElement RF.div_ ["s-dropdown-header", "dropdown-header"]

dropdownHeader_' :: RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownHeader_' = classNameElement' RF.div_ ["s-dropdown-header", "dropdown-header"]

dropdownButton_ :: [ClassName] -> [RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownButton_ = classNameElement RF.div_ ["s-dropdown-toggle", "dropdown-toggle", "btn"]

dropdownButton_' :: RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownButton_' = classNameElement' RF.div_ ["s-dropdown-toggle", "dropdown-toggle", "btn", "btn-secondary"]

dropdownAnchor_ :: [ClassName] -> [RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownAnchor_ = classNameElement RF.div_ ["s-dropdown-toggle", "dropdown-toggle"]

dropdownAnchor_' :: RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdownAnchor_' = classNameElement' RF.div_ ["s-dropdown-toggle", "dropdown-toggle", "btn", "btn-secondary"]

dropdown_ :: DropdownProperty -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
dropdown_ = RF.view dropdown

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

dropdown :: RF.ReactView DropdownProperty
dropdown = RF.defineStatefulView "s-dropdown" defaultDropdownState $ \state props -> do
    let animate = ("s-animate", props^.dropdownPropertyAnimation)
        open = ("open", state^.stateIsOpened)
        classNames = open : animate : map (,True) ("s-dropdown" : "dropdown" : props^.dropdownPropertyClass)
        tabIndex = props^.dropdownPropertyTabIndex
    RF.div_ [RF.classNames classNames, RF.onBlur onBlur, RF.onClick onClick, "tabIndex" @= tabIndex] RF.childrenPassedToView
  where
    onBlur _ _ state = ([], Just $ set stateIsOpened False state)
    onClick _ _ state = ([], Just $ set stateIsOpened (not $ state^.stateIsOpened) state)
