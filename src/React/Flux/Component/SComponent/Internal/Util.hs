{-# LANGUAGE OverloadedStrings #-}
module React.Flux.Component.SComponent.Internal.Util where

import qualified Data.Text                             as T
import           React.Flux                            (($=))
import qualified React.Flux                            as RF
import           React.Flux.Component.SComponent.Types

classNameText :: [ClassName] -> ClassName
classNameText = T.unwords

classNameElement :: ([RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a)
                 -> [ClassName]
                 -> [ClassName]
                 -> [RF.PropertyOrHandler eventHandler]
                 -> RF.ReactElementM eventHandler a
                 -> RF.ReactElementM eventHandler a
classNameElement element embed classes props = element $ ("className" $= className) : props
  where
    className = classNameText $ embed ++ classes

classNameElement' :: ([RF.PropertyOrHandler eventHandler] -> RF.ReactElementM eventHandler a -> RF.ReactElementM eventHandler a)
                  -> [ClassName]
                  -> RF.ReactElementM eventHandler a
                  -> RF.ReactElementM eventHandler a
classNameElement' element embed = element ["className" $= classNameText embed]
