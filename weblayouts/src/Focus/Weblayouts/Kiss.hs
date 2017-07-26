{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Focus.Weblayouts.Kiss  where

import Reflex
import Reflex.Dom.Core
import Reflex.Dom.Path

import Control.Monad.Fix
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Focus.JS.FontAwesome as FA
import Web.FontAwesomeType 

-- | Set of functions to handle website directories/routes
class WebRoute a where 
  routeToTitle :: a -> Text -- ^ returns text to be printed on a Nav Bar's Dom
  routeToUrl :: a -> Text -- ^ returns text to be appended to the URI
  routeToWidget :: (DomBuilder t m) => a -> m () -- ^ returns the corresponding widget
  urlToRoute :: Text -> Maybe a -- ^ opposite of routeToUrl

-- | Body generating function, adds navbar and corresponding widgets
bodyGen :: (DomBuilder t m, PostBuild t m, Prerender js m, MonadHold t m
          , MonadFix m, PerformEvent t m, TriggerEvent t m, WebRoute a, IsPath a, Ord a) 
              => Text  -- ^ path to image in project directory
              -> [a]   -- ^ list of directories/routes of website
              -> a     -- ^ directory/route website starts on initially
              ->  m () 
bodyGen theLogo pageTabs ir = do
  rec
    pageSwitch <- elClass "div" "header" $ do
      rec
        (homeEvent,_) <- elAttr' "img" ("class" =: "logo" <> "src" =: theLogo) blank
      goHome <- return $ (head pageTabs) <$ domEvent Click homeEvent -- ^ go Home if site logo is clicked
      goNavi <- mobileNavMenu (navMenu active pageTabs) active
      return (leftmost [goHome, goNavi])

    active <- prerender (routeToWidget ir >> return (constDyn ir))
      (pathWidget $ \r -> do
        routeToWidget r
        return (pageSwitch, r))
  return ()

----------------------------------------------------NAV MENU BUILDER ---------------------------------------------------
-- | Nav Bar generator produces click-able Widget Events
navMenu :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, WebRoute a, IsPath a, Ord a) => Dynamic t a -> [a] -> m (Event t a)
navMenu currentTab tabList = do
  let currentTabDemux = demux currentTab      -- ^ change type (Dynamic t a) to (Demux t a)
  rec events <- forM tabList $ \route -> do
        let selected = demuxed currentTabDemux route -- ^ compare currentTab and section
        let highlight = zipDynWith isActive currentTab selected -- ^ if selected is True, highlight currentTab
        el "li" $ do
          -- | Get anchor tag element with Route name and corresponding "active:" styling
          (linkEl, _) <- elDynAttr' "a" (highlight) $ text (routeToTitle route)
          return (route <$ domEvent Click linkEl)  -- ^ get Event t Route anchor element
  -- | send clicked Route to \route function
  return $ leftmost events

isActive :: (WebRoute a) => a -> Bool -> Map Text Text
isActive ia isit = "id" =: (routeToTitle ia)
           <> "class" =: (active isit)
  where
    active True = "chosenOne"
    active False = ""

-------------MOBILE NAV MENU BUILDER ----------------------------------
--TODO some of the style changes that are within the style.css file may want
--to be integrated into the function somehow in order to avoid fingering
--through css code to figure out how to get this function to be useful
--straight out of the box.
-- | Make the mobile app Menu become the parent ul of the li generated from
-- 'navMenu'
mobileNavMenu :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, WebRoute a, IsPath a, Ord a)=> m (Event t a) -> Dynamic t a -> m (Event t a)
mobileNavMenu items activeTab = do
  rec
    isOpen <- toggle False onClick                      -- ^ add toggle-able Boolean Event when clicked
    let toggleOpen = section <$> isOpen                 -- ^ fmap Boolean to 'section'
    let onClick = domEvent Click modalDiv                -- ^ add Event target
    (modalDiv,mWidg) <- elDynAttr' "div" toggleOpen $ do -- ^ Bootleg modal div (used to close dropdown if user clicks elsewhere)
      (_,widg) <- elDynAttr' "ul" toggleOpen $ do        -- ^ get a tuple with (EventResult, m())
        let selectedTitle = Text.toUpper . routeToTitle <$> activeTab    -- ^ set Title for Responsive Menu
        el "div" $ text " "                              -- ^ added this div for flexbox fix (temp fix)
        el "p" $ dynText selectedTitle                   -- ^ add h3 with Dynmically changing title
        _ <- FA.faIcon' FaBars $ def {_faConfig_size = Size_Large} -- ^ add FontAwsome Menu Icon with Large size configs added
        items                                             -- ^ add contents of whatever widget is passed as an arg
      return widg
  return (mWidg)

-- | helper function for mobileNavMenu
section :: Bool -> Map Text Text
section True = "class" =: "sections"
section False = "class" =: "noshow"

