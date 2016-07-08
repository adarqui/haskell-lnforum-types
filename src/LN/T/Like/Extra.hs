module LN.T.Like.Extra (
  (+->),
  depListToLower
) where



import           Data.Text    (Text)
import qualified Data.Text    as T (toLower)
import           LN.T.DepList



infixr 9 +->

(+->) :: a -> [a] -> [a]
(+->) a as = a : as



depListToLower :: DepList Text -> DepList Text
depListToLower = map (map T.toLower)
