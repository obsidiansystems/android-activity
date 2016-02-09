{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
module Focus.JS.Transition where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Time.Clock

import Reflex.Dom

data Transition = Transition { transitionProperties :: Map String (String, String, NominalDiffTime), additionalProperties :: Map String String }

transitionTime :: Transition -> NominalDiffTime 
transitionTime = maximum . (0 :) . map (\(_,_,t) -> t) . Map.elems . transitionProperties   

renderCSS :: Map String String -> String
renderCSS m = concat [k ++ ":" ++ v ++ ";" | (k,v) <- Map.toList m]

transitionProp :: Transition -> String
transitionProp = ("transition:" ++) . (++ ";") . intercalate "," . map (\(k,(_,_,t)) -> unwords [k, show t]) . Map.toList . transitionProperties 

transitionStyle' :: ((String,String,NominalDiffTime) -> String) -> Transition -> String
transitionStyle' f tr = transitionProp tr ++ renderCSS (Map.map f (transitionProperties tr)) ++ renderCSS (additionalProperties tr)

transitionStyle0 :: Transition -> String
transitionStyle0 = transitionStyle' (\(v0,_,_) -> v0)

transitionStyle1 :: Transition -> String
transitionStyle1 = transitionStyle' (\(_,v1,_) -> v1)

fadeOutRight, fadeInLeft :: NominalDiffTime -> Transition
fadeOutRight n = Transition ("opacity" =: ("1","0", n) <> "left" =: ("0%","10%",n)) ("position" =: "relative")
fadeInLeft n = Transition ("opacity" =: ("0","1", n) <> "left" =: ("-10%","0%",n)) ("position" =: "relative")

transitionWorkflow :: forall t m a. (MonadWidget t m) => Transition -> Transition -> Workflow t m a -> Workflow t m a
transitionWorkflow hello goodbye = f where
  f :: Workflow t m a -> Workflow t m a
  f (Workflow act) = Workflow $ do
    do hello0 <- delay 0.01 =<< getPostBuild
       rec attrs <- mapDyn ("style" =:) =<<
                      holdDyn (transitionStyle0 hello)
                              (leftmost [(transitionStyle1 hello) <$ hello0
                                        ,(transitionStyle0 goodbye) <$ goodbye0
                                        ,(transitionStyle1 goodbye) <$ goodbye1])
           (r,e) <- elDynAttr "div" (traceDyn "attrs" attrs) act
           let goodbye0 = fmap f e
           goodbye1 <- delay 0.01 goodbye0
           end <- delay (transitionTime goodbye) goodbye0
       return (r, end)

