module Utils ((=$)) where
import Data.Enumerator

(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
(=$) = (joinI .) . ($$)

infixr 0 =$
