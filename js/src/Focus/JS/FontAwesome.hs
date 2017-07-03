{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Focus.JS.FontAwesome where

import Data.Map (Map)
import Reflex.Dom.Core
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Data.Default
import Data.Maybe

import Web.FontAwesomeType -- ^ FontAwesome Enumerations

{- | The Size, Pull, Animation, Rotation, Flip data types are available to
 - define configurable Font Awesome options.  
-}

data Size = Size_Default
          | Size_Large -- ^ Make icon 33% bigger
          | Size_2x    -- ^ Double icon size
          | Size_3x    -- ^ Triple icon size
          | Size_4x    -- ^ Cuadruple icon size
          | Size_5x    -- ^ Quintuple icon size

data Pull = Pull_Left  -- ^ Float left 
          | Pull_Right -- ^ Float right

data Animation = Animation_Spin
               | Animation_Pulse

data Rotation = Rotate_90
              | Rotate_180
              | Rotate_270

data Flip = Flip_Horizontal
          | Flip_Vertical

{- | Use the FAConfig data type to create custom instances of how you would
 - like your icon to appear/behave. 
-}
data FAConfig = FAConfig
  { _faConfig_size :: Size
  , _faConfig_fixedWidth :: Bool
  , _faConfig_border :: Bool
  , _faConfig_pull :: Maybe Pull
  , _faConfig_animation :: Maybe Animation
  , _faConfig_rotate :: Maybe Rotation
  , _faConfig_flip :: Maybe Flip
	, _faConfig_listIcon :: Bool
  }

-- ^ Use the default FAConfig instance "def" to simply display an icon normally. 
instance Default FAConfig where
  def = FAConfig
    { _faConfig_size = Size_Default
    , _faConfig_fixedWidth = False
    , _faConfig_border = False
    , _faConfig_pull = Nothing
    , _faConfig_animation = Nothing
    , _faConfig_rotate = Nothing
    , _faConfig_flip = Nothing
		, _faConfig_listIcon = False
    }

{- | This function takes an FAConfig type and generated the necessary "fa"
  -- class names for desired icon behavior
-}
faConfigClass :: FAConfig -> Text
faConfigClass c = T.intercalate " " . catMaybes $
  [ Just " fa"
  , case _faConfig_size c of
         Size_Default -> Nothing
         Size_Large -> Just "fa-lg"
         Size_2x -> Just "fa-2x"
         Size_3x -> Just "fa-3x"
         Size_4x -> Just "fa-4x"
         Size_5x -> Just "fa-5x"
  , if _faConfig_fixedWidth c then Just "fa-fw" else Nothing
  , if _faConfig_border c then Just "fa-border" else Nothing
  , case _faConfig_pull c of
         Just Pull_Right -> Just "fa-pull-right"
         Just Pull_Left -> Just "fa-pull-left"
         Nothing -> Nothing
  , case _faConfig_animation c of
         Just Animation_Pulse -> Just "fa-pulse"
         Just Animation_Spin -> Just "fa-spin"
         Nothing -> Nothing
  , case _faConfig_rotate c of
         Just Rotate_90 -> Just "fa-rotate-90"
         Just Rotate_180 -> Just "fa-rotate-180"
         Just Rotate_270 -> Just "fa-rotate-270"
         Nothing -> Nothing
  , case _faConfig_flip c of
         Just Flip_Horizontal -> Just "fa-flip-horizontal"
         Just Flip_Vertical -> Just "fa-flip-vertical"
         Nothing -> Nothing
	, if _faConfig_listIcon c then Just "fa-li" else Nothing
  ]

{- | This function is used to generate a <link> tag that references MaxCDN
 - bootstrap content.
 -}
fontAwesomeCDN :: DomBuilder t m => m ()
fontAwesomeCDN = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css") $ return ()

dynIconAttr :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Text -> m ()
dynIconAttr m i = do
  let attr = ffor i $ \name -> m <> if T.null name then mempty else "class" =: ("fa fa-" <> name)
  elDynAttr "i" attr $ return ()

dynIcon2xAttr :: (DomBuilder t m, PostBuild t m) => Map Text Text -> Dynamic t Text -> m ()
dynIcon2xAttr m i = do
  let attr = ffor i $ \name -> m <> if T.null name then mempty else "class" =: ("fa fa-2x fa-" <> name)
  elDynAttr "i" attr $ return ()

dynIcon :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
dynIcon = dynIconAttr mempty

dynIcon2x :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
dynIcon2x = dynIcon2xAttr mempty

{- | Warning: This function has been depreciated, please faIcon functions
 - instead -}
icon :: DomBuilder t m => Text -> m ()
icon i = elClass "i" ("fa fa-" <> i) $ return ()

icon1g :: DomBuilder t m => Text -> m ()
icon1g i = icon (i <> " fa-1g")

icon2x :: DomBuilder t m => Text -> m ()
icon2x i = icon (i <> " fa-2x")

icon3x :: DomBuilder t m => Text -> m ()
icon3x i = icon (i <> " fa-3x")

icon4x :: DomBuilder t m => Text -> m ()
icon4x i = icon (i <> " fa-4x")

icon5x :: DomBuilder t m => Text -> m ()
icon5x i = icon (i <> " fa-5x")

-- ^ icon prime functions
icon' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t, ())
icon' i = elClass' "i" ("fa fa-" <> i) $ return ()

icon1g' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t, ())
icon1g' i = icon' (i <> " fa-1g")

icon2x' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t, ())
icon2x' i = icon' (i <> " fa-2x")

icon3x' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t, ())
icon3x' i = icon' (i <> " fa-3x")

icon4x' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t, ())
icon4x' i = icon' (i <> " fa-4x")

icon5x' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t, ())
icon5x' i = icon' (i <> " fa-5x")

-- ^ Type checked faIcon functions
faIcon :: DomBuilder t m => FontAwesome -> FAConfig -> m ()
faIcon i conf = elClass "i" ((faPack i) <> (faConfigClass conf)) $ return ()

faIcon1g :: DomBuilder t m => FontAwesome -> FAConfig -> m ()
faIcon1g i conf = icon1g $ drop3class i <> faConfigClass conf

faIcon2x :: DomBuilder t m => FontAwesome -> FAConfig -> m ()
faIcon2x i conf = icon2x $ drop3class i <> faConfigClass conf

faIcon3x :: DomBuilder t m => FontAwesome -> FAConfig -> m ()
faIcon3x i conf = icon3x $ drop3class i <> faConfigClass conf

faIcon4x :: DomBuilder t m => FontAwesome -> FAConfig -> m ()
faIcon4x i conf = icon4x $ drop3class i <> faConfigClass conf

faIcon5x :: DomBuilder t m => FontAwesome -> FAConfig -> m ()
faIcon5x i conf = icon5x $ drop3class i <> faConfigClass conf

-- ^ faIcon prime functions 
faIcon' :: DomBuilder t m => FontAwesome -> FAConfig -> m (Element EventResult (DomBuilderSpace m) t, ())
faIcon' i conf = elClass' "i" ((faPack i) <> (faConfigClass conf)) $ return ()

faIcon1g' :: DomBuilder t m => FontAwesome -> FAConfig -> m (Element EventResult (DomBuilderSpace m) t, ())
faIcon1g' i conf = icon1g' $ drop3class i <> faConfigClass conf

faIcon2x' :: DomBuilder t m => FontAwesome -> FAConfig -> m (Element EventResult (DomBuilderSpace m) t, ())
faIcon2x' i conf = icon2x' $ drop3class i <> faConfigClass conf

faIcon3x' :: DomBuilder t m => FontAwesome -> FAConfig -> m (Element EventResult (DomBuilderSpace m) t, ())
faIcon3x' i conf = icon3x' $ drop3class i <> faConfigClass conf

faIcon4x' :: DomBuilder t m => FontAwesome -> FAConfig -> m (Element EventResult (DomBuilderSpace m) t, ())
faIcon4x' i conf = icon4x' $ drop3class i <> faConfigClass conf

faIcon5x' :: DomBuilder t m => FontAwesome -> FAConfig -> m (Element EventResult (DomBuilderSpace m) t, ())
faIcon5x' i conf = icon5x' $ drop3class i <> faConfigClass conf

-- helper functions --
drop3class :: FontAwesome -> Text
drop3class = T.drop 3 . T.pack . fontAwesomeClass

faPack :: FontAwesome -> Text
faPack = T.pack . fontAwesomeClass

--TODO Consider creating a function that can create an <ul> of font awesome <li>.
--faUnorderedList :: DomBuilder t m => [(FontAwesome,FAConfig)] -> m()
