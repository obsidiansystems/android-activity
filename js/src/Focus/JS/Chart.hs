{-# LANGUAGE RecursiveDo, OverloadedStrings, ScopedTypeVariables #-}

module Focus.JS.Chart (pie) where

import Control.Monad
import Data.List
import Reflex.Dom
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class

import GHCJS.DOM.Element (setInnerHTML)

tshow :: Show a => a -> Text
tshow = T.pack . show

{- -- Right now, this sadly crashes reflex-dom because it expects to be able to cast all elements to HTMLElements, but this will result in an SVGElement.
svgAttr :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m a
svgAttr name attrs body = snd <$> element name (def & namespace .~ svgNS & initialAttributes .~ Map.mapKeysMonotonic (\x -> AttributeName Nothing x) attrs) body
  where svgNS = Just "http://www.w3.org/2000/svg"
-}

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
  liftIO (setInnerHTML (_element_raw divEl) (Just theSVG))
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
    cvs = filter (\(cs,v) -> v > 0) cvs'
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
    arcPoints = pointsAt r endAngles
    textPoints = pointsAt ((r + ri) / 2) middles
    wedges = zipWith3 (\a b p ->
                          (\bg -> if p < 100
                                    then "<path d=\"" <> T.unwords ["M", pair c, "L", pair a, "A", pair c, "0", "0", "0", pair b, "L", pair c, "Z"]
                                         <> "\" fill=\"" <> bg <> "\"></path>"
                                    else "<circle cx=\"" <> tshow (w/2) <> "\" cy=\"" <> tshow (w/2) <> "\" r = \"" <> tshow r
                                         <> "\" fill=\"" <> bg <> "\"></circle>"))
                      arcPoints (tail arcPoints) pcts