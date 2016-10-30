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

tshow :: Show a => a -> Text
tshow = T.pack . show

svgAttr :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m a
svgAttr name attrs body = snd <$> element name (def & namespace .~ svgNS & initialAttributes .~ Map.mapKeysMonotonic (\x -> AttributeName Nothing x) attrs) body
  where svgNS = Just "http://www.w3.org/2000/svg"

-- | Takes a width, and a list of pairs of (foreground, background) colour pairs, and numerical values, and constructs an SVG pie chart for those values, having the given colours.
pie :: MonadWidget t m => (Floating a, RealFrac a, Show a) => a -> [((Text, Text), a)] -> m ()
pie w cvs = do
  el "div" $ do
    svgAttr "svg" ("width" =: tshow (ceiling w :: Integer) <> "height" =: tshow (ceiling w :: Integer)) $ do
      forM_ (zip colors wedges) $ \((_, bg), wedge) -> do
        svgAttr "path" ("d" =: wedge <> "fill" =: bg) blank
      svgAttr "circle" ("cx" =: tshow (w/2) <> "cy" =: tshow (w/2) <> "r" =: tshow (w/3) <> "fill" =: "#fff") blank
      forM_ (zip3 colors textPoints pcts) $ \((fg, _), (tx, ty), pct) -> do
        svgAttr "text" ("x" =: tshow tx <> "y" =: tshow ty
                      <> "style" =: ("font-size:" <> tshow fontSize <> "px;" <> "fill:" <> fg <> ";" <> "text-anchor: middle; dominant-baseline: middle;")) (text pct)
  where
    colors = map fst cvs
    vs = map snd cvs
    r = w/2
    ri = (2/3)*r
    fontSize = (r - ri) / 2.5
    c = (r, r)
    pair (x,y) = T.pack (show x <> "," <> show y)
    s = sum vs
    fractions = [v/s | v <- scanl (+) 0 vs]
    pcts = [tshow (round ((v/s)*100) :: Integer) <> "%" | v <- vs]
    endAngles = [2*pi*f | f <- fractions]
    middles = zipWith (\u v -> (u+v)/2) endAngles (tail endAngles)
    pointsAt radius angles = [(r + radius * sin t, r + radius * cos t) | t <- angles]
    arcPoints = pointsAt r endAngles
    textPoints = pointsAt ((r + ri) / 2) middles
    wedges = zipWith (\u v -> T.unwords ["M", pair c, "L", pair u, "A", pair c, "0", "0", "0", pair v, "L", pair c, "Z"]) arcPoints (tail arcPoints)