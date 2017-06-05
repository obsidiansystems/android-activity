module Focus.AppendMap (module X, (=:)) where

import Data.AppendMap as X

-- | Operator for creating a singleton 'Map'
(=:) :: k -> a -> AppendMap k a
k =: v = singleton k v
infixr 7 =:
