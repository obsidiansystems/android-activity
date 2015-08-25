module Focus.Misc where

isRight x = case x of
  Left _ -> False
  Right _ -> True

trisequence :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
trisequence abc = case abc of
                       (Just x, Just y, Just z) -> Just (x, y, z)
                       _ -> Nothing


