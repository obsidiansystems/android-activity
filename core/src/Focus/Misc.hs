module Focus.Misc where

isRight x = case x of
  Left _ -> False
  Right _ -> True
