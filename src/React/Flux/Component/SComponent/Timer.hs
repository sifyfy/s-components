{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
module React.Flux.Component.SComponent.Timer
    ( -- * Property
      Seconds
    , TimerViewType (..)
    , TimerProperty (..)
    , timerPropertySeconds
    , timerPropertyViewType
    , timerPropertySecondsPrefix
    , timerPropertySecondsSuffix
    , timerPropertyMinutesPrefix
    , timerPropertyMinutesSuffix
    , timerPropertyHoursPrefix
    , timerPropertyHoursSuffix
    , defaultTimerProperty
      -- * Element
    , timer_
    , timer_'
      -- * View
    , timer
    ) where

import           Control.Lens
import           Data.Monoid
import           Data.Text                                       ()
import           Data.Word                                       (Word)
import           React.Flux                                      (($=), (@=))
import qualified React.Flux                                      as RF
import           React.Flux.Component.SComponent.Internal.Import (Generic, NFData, Typeable)
import           React.Flux.Component.SComponent.Internal.Util   (classNameElement')

--------------------------------------------------------------------------------
-- Property
--------------------------------------------------------------------------------

type Seconds = Word

data TimerViewType
    = TimerOnlySeconds
    | TimerOnlyMinutes
    | TimerOnlyHours
    | TimerMinutesSeconds
    | TimerHoursMinutesSeconds
    deriving (Show, Enum, Typeable, Generic, NFData)

data TimerProperty = TimerProperty
    { _timerPropertySeconds       :: !Seconds
    , _timerPropertyViewType      :: !TimerViewType
    , _timerPropertyHoursPrefix   :: String
    , _timerPropertyHoursSuffix   :: String
    , _timerPropertyMinutesPrefix :: String
    , _timerPropertyMinutesSuffix :: String
    , _timerPropertySecondsPrefix :: String
    , _timerPropertySecondsSuffix :: String
    } deriving (Show, Typeable, Generic, NFData)

makeLenses ''TimerProperty

defaultTimerProperty :: TimerProperty
defaultTimerProperty = TimerProperty 0 TimerMinutesSeconds "" ":" "" ":" "" ""

--------------------------------------------------------------------------------
-- Element
--------------------------------------------------------------------------------

timer_ :: Seconds -> RF.ReactElementM eventHandler ()
timer_ s = RF.view timer (timerPropertySeconds .~ s $ defaultTimerProperty) mempty

timer_' :: TimerProperty -> RF.ReactElementM eventHandler ()
timer_' props = RF.view timer props mempty

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

type Minutes = Word
type Hours = Word

minutes :: Seconds -> (Minutes, Seconds)
minutes = (`divMod` 60)

hours :: Minutes -> (Hours, Minutes)
hours = (`divMod` 60)

cut :: Word -> String -> String
cut 0 _ = ""
cut _ s = s

base :: String -> RF.ReactElementM eventHandler ()
base = classNameElement' RF.span_ ["s-timer"] . RF.elemText

onlySeconds :: TimerProperty -> RF.ReactElementM eventHandler ()
onlySeconds props = base $ concat
    [ props^.timerPropertySecondsPrefix
    , show $ props^.timerPropertySeconds
    , props^.timerPropertySecondsSuffix
    ]

onlyMinutes :: TimerProperty -> RF.ReactElementM eventHandler ()
onlyMinutes props = base $ concat
    [ props^.timerPropertyMinutesPrefix
    , show $ fst $ minutes $ props^.timerPropertySeconds
    , props^.timerPropertyMinutesSuffix
    ]

onlyHours :: TimerProperty -> RF.ReactElementM eventHandler ()
onlyHours props = base $ concat
    [ props^.timerPropertyHoursPrefix
    , show $ fst $ hours $ fst $ minutes $ props^.timerPropertySeconds
    , props^.timerPropertyHoursSuffix
    ]

minutesSeconds :: TimerProperty -> RF.ReactElementM eventHandler ()
minutesSeconds props = base $ concat
    [ props^.timerPropertyMinutesPrefix
    , show m
    , props^.timerPropertyMinutesSuffix
    , props^.timerPropertySecondsPrefix
    , show s
    , props^.timerPropertySecondsSuffix
    ]
  where
    (m,s) = minutes $ props^.timerPropertySeconds

hoursMinutesSeconds :: TimerProperty -> RF.ReactElementM eventHandler ()
hoursMinutesSeconds props = base $ concat
    [ props^.timerPropertyHoursPrefix
    , show h
    , props^.timerPropertyHoursSuffix
    , props^.timerPropertyMinutesPrefix
    , show m
    , props^.timerPropertyMinutesSuffix
    , props^.timerPropertySecondsPrefix
    , show s
    , props^.timerPropertySecondsSuffix
    ]
  where
    (m0,s) = minutes $ props^.timerPropertySeconds
    (h,m) = hours m0

timer :: RF.ReactView TimerProperty
timer = RF.defineView "s-timer" $ \props ->
    case props^.timerPropertyViewType of
        TimerOnlySeconds         -> onlySeconds props
        TimerOnlyMinutes         -> onlyMinutes props
        TimerOnlyHours           -> onlyHours props
        TimerMinutesSeconds      -> minutesSeconds props
        TimerHoursMinutesSeconds -> hoursMinutesSeconds props
