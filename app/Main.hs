{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}
module Main where

import           Control.Lens
import           Data.Text                       (Text, unpack)
import           React.Flux                      (($=), (@=))
import qualified React.Flux                      as RF
import           React.Flux.Component.SComponent
import           Text.Shakespeare.Text           (st)

github, twitter :: Text
github = "https://github.com/siphilia/s-components"
twitter = "https://twitter.com/siphilia_rn"

header_ :: RF.ReactElementM eventHandler ()
header_ = RF.header_ ["className" $= "navbar navbar-full navbar-light bg-faded box-shadow"] $ do
    RF.a_ ["className" $= "navbar-brand", "href" $= "#"] "s-component demo"
    RF.nav_ ["className" $= "nav navbar-nav pull-xs-right"] $
        RF.a_ ["className" $= "nav-item nav-link", "href" $= github ] "GitHub"

footer_ :: RF.ReactElementM eventHandler ()
footer_ = RF.footer_ ["className" $= "footer text-muted"] $
    RF.div_ $ do
        RF.a_ ["href" $= github] "GitHub"
        ", Author:"
        RF.a_ ["href" $= twitter] "@siphilia_rn"
        ", Code licensed BSD3."

section_ :: Text -> String -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
section_ id_ h children = RF.section_ ["className" $= "component", "id" $= id_] $ do
    RF.h1_ ["className" $= "name"] $ RF.elemText h
    RF.cldiv_ "content" children

previewAndCode_ :: String -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a
previewAndCode_ title preview code = RF.cldiv_ "card" $ do
    RF.cldiv_ "card-header" $ RF.elemText title
    RF.cldiv_ "card-block" preview
    RF.cldiv_ "card-code" code

dropdownPreview_ :: RF.ReactElementM eventHandler ()
dropdownPreview_ = do
    basic_
    animation_
  where
    dropdownA_ props = dropdown_ props $ do
        dropdownButton_' "Dropdown"
        dropdownMenu_' $ do
            dropdownItem_' "Item A"
            dropdownItem_' "Item B"
            dropdownDivider_'
            dropdownItem_' "Item C"
            dropdownDivider_'
            dropdownHeader_' "Header in dropdown"
            dropdownItem_' "Item D"
            dropdownItem_' "Item E"

    basic_ = previewAndCode_ "Basic" (dropdownA_ defaultDropdownProperty) $
        RF.pre_ $
            RF.elemText $ unpack [st|import React.Flux.Component.SComponent

dropdownA_ :: ReactElementM eventHandler ()
dropdownA_ = dropdown_ defaultDropdownProperty $ do
    dropdownButton_' "Dropdown"
    dropdownMenu_' $ do
        dropdownItem_' "Item A"
        dropdownItem_' "Item B"
        dropdownDivider_'
        dropdownItem_' "Item C"
        dropdownDivider_'
        dropdownHeader_' "Header in dropdown"
        dropdownItem_' "Item D"
        dropdownItem_' "Item E"
|]

    animation_ = previewAndCode_ "Animation" (dropdownA_ (set dropdownPropertyAnimation True defaultDropdownProperty)) $
        RF.pre_ $
            RF.elemText $ unpack [st|import Control.Lens
import React.Flux.Component.SComponent

dropdownA_ :: ReactElementM eventHandler ()
dropdownA_ = dropdown_ (set dropdownPropertyAnimation True defaultDropdownProperty) $ do
    dropdownButton_' "Dropdown"
    dropdownMenu_' $ do
        dropdownItem_' "Item A"
        dropdownItem_' "Item B"
        dropdownDivider_'
        dropdownItem_' "Item C"
        dropdownDivider_'
        dropdownHeader_' "Header in dropdown"
        dropdownItem_' "Item D"
        dropdownItem_' "Item E"
|]

main_ :: RF.ReactElementM eventHandler ()
main_ = RF.main_ $
    section_ "dropdown" "Dropdown" dropdownPreview_

sidenav_ :: RF.ReactElementM eventHandler ()
sidenav_ = do
    RF.h1_ ["className" $= "sidenav-header"] "Components"
    RF.nav_ ["className" $= "sidenav-nav nav nav-stacked"] $
        navItem "#dropdown" "Dropdown"
  where
    navItem href = RF.a_ ["className" $= "nav-item nav-link", "href" $= href]

demo :: RF.ReactView ()
demo = RF.defineView "demo" $ \props -> RF.div_ $ do
    header_
    RF.cldiv_ "container-fluid main" $
        RF.cldiv_ "row" $ do
            RF.cldiv_ "col-lg-10 col-md-9 col-xs-12" main_
            RF.cldiv_ "col-lg-2 col-md-3 col-xs-12" sidenav_
    footer_

main :: IO ()
main = RF.reactRender "app" demo ()
