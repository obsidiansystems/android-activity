{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Focus.Weblayouts.Kiss where

import Reflex
import Reflex.Dom
import Reflex.Dom.Path

import qualified Data.Map as Map
import Data.Monoid
import Control.Monad
import Data.Map (Map)
import Data.Text (Text)

import Focus.JS.Prerender (Prerender, prerender)
import Control.Monad.Fix

import Focus.JS.FontAwesome as FA
import Web.FontAwesomeType

-- TODO You need Common.Route data types and the Route functions
-- TODO Consider creating a type class that will allow an individual to defined Route Data types, assign them String values
-- and be able to manipulate and be able to append them to the URI and use them in the website's navbar

bodyGen :: (DomBuilder t m, PostBuild t m, Prerender js m, MonadHold t m, MonadFix m, PerformEvent t m, TriggerEvent t m) 
							=> Text -> [Route] -> Route ->  m ()
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

    footer  
  return ()

-- | Nav Bar generator produces click-able Widget Events
navMenu :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Dynamic t Route -> [Route] -> m (Event t Route)
navMenu currentTab tabList = do
  let currentTabDemux = demux currentTab      -- ^ change type (Dynamic t Route) to (Demux t Route)
  rec events <- forM tabList $ \route -> do
        let selected = demuxed currentTabDemux route -- ^ compare currentTab and section
        let highlight = zipDynWith isActive currentTab selected -- ^ if selected is True, highlight currentTab
        el "li" $ do
          -- | Get anchor tag element with Route name and corresponding "active:" styling
          (linkEl, _) <- elDynAttr' "a" (highlight) $ text (routeToTitle route)
          return (route <$ domEvent Click linkEl)  -- ^ get Event t Route anchor element
  -- | send clicked Route to \route function
  return $ leftmost events

--TODO some of the style changes that are within the style.css file may want
--to be integrated into the function somehow in order to avoid fingering
--through css code to figure out how to get this function to be useful
--straight out of the box.
-- | Make the mobile app Menu become the parent ul of the li generated from
-- 'navMenu'
mobileNavMenu :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)=> m (Event t Route) -> Dynamic t Route -> m (Event t Route)
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

isActive :: Route -> Bool -> Map Text Text
isActive ia isit = "id" =: (routeToTitle ia)
           <> "style" =: ("border-bottom: " <> active isit)
  where
    active True = "4px solid #CCC"
    active False = "none;"

-- | Produces Text for navMenu, takes a route as an arguement
routeToTitle :: Route -> Text
routeToTitle r = case r of
     Route_AboutUs -> "About Us"
     Route_Technology -> "Technology"
     Route_DataAnalysis -> "Past Work: Data Analysis"
     Route_GroupMessaging -> "Past Work: Group Messaging"
     Route_Logistics -> "Past Work: Logistics"

-- | Receives a route and returns it's corresponding widget 
routeToWidget :: DomBuilder t m => Route -> m ()
routeToWidget r = case r of
     Route_AboutUs -> aboutUs
     Route_Technology -> technology
     Route_DataAnalysis -> dataAnalysis
     Route_GroupMessaging -> groupMessaging
     Route_Logistics -> logistics

-- | helper function for mobileNavMenu
section :: Bool -> Map Text Text
section True = "class" =: "sections"
section False = "class" =: "noshow"
