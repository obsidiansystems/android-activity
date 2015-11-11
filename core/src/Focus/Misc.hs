module Focus.Misc where

isRight :: Either a b -> Bool
isRight x = case x of
  Left _ -> False
  Right _ -> True

trisequence :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
trisequence abc = case abc of
                       (Just x, Just y, Just z) -> Just (x, y, z)
                       _ -> Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c



