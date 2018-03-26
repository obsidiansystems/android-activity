{-# LANGUAGE RecursiveDo, OverloadedStrings, ScopedTypeVariables #-}

module Focus.JS.Chart (pie, pie2) where

import Reflex.Dom.Core
import Control.Monad
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

import GHCJS.DOM.Element (setInnerHTML)

tshow :: Show a => a -> Text
tshow = T.pack . show

{- -- Right now, this sadly crashes reflex-dom because it expects to be able to cast all elements to HTMLElements, but this will result in an SVGElement.
-}
svgAttr :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m a
svgAttr name attrs body = fmap snd $ element name attrs' body
  where
    svgNS = Just "http://www.w3.org/2000/svg"
    attrs' = def
      & namespace .~ svgNS
      & initialAttributes .~ Map.mapKeysMonotonic (\x -> AttributeName Nothing x) attrs

-- | Takes a width, and a list of pairs of (foreground, background) colour pairs, and numerical values, and constructs an SVG pie chart for those values, having the given colours.
pie :: MonadWidget t m => (Floating a, RealFrac a, Show a) => Map Text Text -> a -> [((Text, Text), a)] -> m ()
pie attrs w cvs' = do
  (divEl, _) <- elAttr' "div" attrs blank
  let theSVG = "<svg width=\"" <> tshow (ceiling w :: Integer) <> "\" height=\"" <> tshow (ceiling w :: Integer) <> "\">"
               <> mconcat [wedge bg | ((_,bg),wedge) <- zip colors wedges]
               <> "<circle cx=\"" <> tshow (w/2) <> "\" cy=\"" <> tshow (w/2) <> "\" r=\"" <> tshow (w/3) <> "\" fill=\"white\"></circle>"
               <> mconcat ["<text x=\"" <> tshow tx <> "\" y=\"" <> tshow ty <> "\" style=\""
                            <> "font-size:" <> tshow fontSize <> "px;" <> "fill:" <> fg <> ";"
                            <> "font-weight: bold;"
                            <> "text-anchor: middle; dominant-baseline: middle;\">" <> tshow pct <> "%</text>"
                          | ((fg, _), (tx, ty), pct) <- zip3 colors textPoints pcts]
               <> "</svg>"
  setInnerHTML (_element_raw divEl) theSVG
  {-
  el "div" $ do
    svgAttr "svg" ("width" =: tshow (ceiling w :: Integer) <> "height" =: tshow (ceiling w :: Integer)) $ do
      forM_ (zip colors wedges) $ \((_, bg), wedge) -> do
        svgAttr "path" ("d" =: wedge <> "fill" =: bg) blank
      svgAttr "circle" ("cx" =: tshow (w/2) <> "cy" =: tshow (w/2) <> "r" =: tshow (w/3) <> "fill" =: "#fff") blank
      forM_ (zip3 colors textPoints pcts) $ \((fg, _), (tx, ty), pct) -> do
        svgAttr "text" ("x" =: tshow tx <> "y" =: tshow ty
                      <> "style" =: ("font-size:" <> tshow fontSize <> "px;" <> "fill:" <> fg <> ";" <> "text-anchor: middle; dominant-baseline: middle;")) (text pct)
  -}
  where
    cvs = filter (\(_,v) -> v > 0) cvs'
    colors = map fst cvs
    vs = map snd cvs
    r = w/2
    ri = (2/3)*r
    fontSize = (r - ri) / 2.5
    c = (r, r)
    pair (x,y) = T.pack (show x <> "," <> show y)
    s = sum vs
    pcts = [round ((v/s)*100) :: Integer | v <- vs]
    fractions = [v/s | v <- scanl (+) 0 vs]
    endAngles = [2*pi*f | f <- fractions]
    middles = zipWith (\u v -> (u+v)/2) endAngles (tail endAngles)
    pointsAt radius angles = [(r + radius * sin t, r + radius * cos t) | t <- angles]
    trails = [pointsAt r [(t * u + (1-t) * v) | t <- [0, 1/5, 2/5, 3/5, 4/5, 1]] | (u,v) <- zip (tail endAngles) endAngles]
    textPoints = pointsAt ((r + ri) / 2) middles
    wedges = zipWith (\(a:bs) p ->
                         (\bg -> if p < 100
                                   then "<path d=\"" <> (T.unwords $ ["M", pair c, "L", pair a]
                                                                  <> concat [["A", pair c, "0", "0", "0", pair b] | b <- bs]
                                                                  <> ["L", pair c, "Z"])
                                        <> "\" fill=\"" <> bg <> "\"></path>"
                                   else "<circle cx=\"" <> tshow (w/2) <> "\" cy=\"" <> tshow (w/2) <> "\" r = \"" <> tshow r
                                        <> "\" fill=\"" <> bg <> "\"></circle>"))
                      trails pcts


-- | Takes a width, and a list of pairs of (foreground, background) colour pairs, and numerical values, and constructs an SVG pie chart for those values, having the given colours.
pie2 :: forall t m a. DomBuilder t m => (Floating a, RealFrac a, Show a) => Map Text Text -> a -> [((Text, Text), a)] -> m ()
pie2 attrs w cvs' = do
  elAttr "div" attrs $ do
    svgAttr "svg" ("width" =: tshow (ceiling w :: Integer) <> "height" =: tshow (ceiling w :: Integer)) $ do
      forM_ (zip colors wedges) $ \((_, bg), wedge) -> do
        wedge bg
      svgAttr "circle" ("cx" =: tshow (w/2) <> "cy" =: tshow (w/2) <> "r" =: tshow (w/3) <> "fill" =: "#fff") blank
      forM_ (zip3 colors textPoints pcts) $ \((fg, _), (tx, ty), pct) -> do
        svgAttr "text" ("x" =: tshow tx <> "y" =: tshow ty <> "style" =: ("font-size:" <> tshow fontSize <> "px;" <> "fill:" <> fg <> ";" <> "text-anchor: middle; dominant-baseline: middle;")) $
          text $ tshow pct <> "%"
  where
    cvs = filter (\(_,v) -> v > 0) cvs'
    colors = map fst cvs
    vs = map snd cvs
    r = w/2
    ri = (2/3)*r
    fontSize = (r - ri) / 2.5
    c = (r, r)
    pair (x,y) = T.pack (show x <> "," <> show y)
    s = sum vs
    pcts = [round ((v/s)*100) :: Integer | v <- vs]
    fractions = [v/s | v <- scanl (+) 0 vs]
    endAngles = [2*pi*f | f <- fractions]
    middles = zipWith (\u v -> (u+v)/2) endAngles (tail endAngles)
    pointsAt radius angles = [(r + radius * sin t, r + radius * cos t) | t <- angles]
    trails = [pointsAt r [(t * u + (1-t) * v) | t <- [0, 1/5, 2/5, 3/5, 4/5, 1]] | (u,v) <- zip (tail endAngles) endAngles]
    textPoints = pointsAt ((r + ri) / 2) middles
    wedges :: [Text -> m ()]
    wedges = zipWith
      (\(a:bs) p bg ->
        if p < 100
          then svgAttr "path" (mconcat [ "d" =: (T.unwords $ ["M", pair c, "L", pair a] <> concat [["A", pair c, "0", "0", "0", pair b] | b <- bs] <> ["L", pair c, "Z"])
                                       , "fill" =: bg
                                       ]) (return ())
          else svgAttr "circle" ("cx" =: tshow (w/2) <> "cy" =: tshow (w/2) <> "r" =: tshow r <> "fill" =: bg) (return ()))
      trails pcts
