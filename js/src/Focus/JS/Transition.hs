{-# LANGUAGE ScopedTypeVariables, RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Focus.JS.Transition where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T

import Reflex.Dom

data Transition = Transition { transitionProperties :: Map Text (Text, Text, NominalDiffTime), additionalProperties :: Map Text Text }

transitionTime :: Transition -> NominalDiffTime 
transitionTime = maximum . (0 :) . map (\(_,_,t) -> t) . Map.elems . transitionProperties   

renderCSS :: Map Text Text -> Text
renderCSS m = mconcat [k <> ":" <> v <> ";" | (k,v) <- Map.toList m]

transitionProp :: Transition -> Text
transitionProp = ("transition:" <>) . (<> ";") . T.intercalate "," . map (\(k,(_,_,t)) -> T.unwords [k, T.pack $ show t]) . Map.toList . transitionProperties 

transitionStyle' :: ((Text,Text,NominalDiffTime) -> Text) -> Transition -> Text
transitionStyle' f tr = transitionProp tr <> renderCSS (Map.map f (transitionProperties tr)) <> renderCSS (additionalProperties tr)

transitionStyle0 :: Transition -> Text
transitionStyle0 = transitionStyle' (\(v0,_,_) -> v0)

transitionStyle1 :: Transition -> Text
transitionStyle1 = transitionStyle' (\(_,v1,_) -> v1)

fadeOutRight, fadeInLeft :: NominalDiffTime -> Transition
fadeOutRight n = Transition ("opacity" =: ("1","0", n) <> "left" =: ("0%","10%",n)) ("position" =: "relative")
fadeInLeft n = Transition ("opacity" =: ("0","1", n) <> "left" =: ("-10%","0%",n)) ("position" =: "relative")

transitionWorkflow :: forall t m a. (DomBuilder t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadFix m, MonadHold t m, MonadIO (Performable m)) => Transition -> Transition -> Workflow t m a -> Workflow t m a
transitionWorkflow hello goodbye = f where
  f :: Workflow t m a -> Workflow t m a
  f (Workflow act) = Workflow $ do
    do hello0 <- delay 0.01 =<< getPostBuild
       rec attrs <- mapDyn ("style" =:) =<<
                      holdDyn (transitionStyle0 hello)
                              (leftmost [(transitionStyle1 hello) <$ hello0
                                        ,(transitionStyle0 goodbye) <$ goodbye0
                                        ,(transitionStyle1 goodbye) <$ goodbye1])
           (r,e) <- elDynAttr "div" attrs act
           let goodbye0 = fmap f e
           goodbye1 <- delay 0.01 goodbye0
           end <- delay (transitionTime goodbye) goodbye0
       return (r, end)

