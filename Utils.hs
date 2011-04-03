module Utils ((=$)) where
import qualified Data.Enumerator as E

(=$) :: Monad m => E.Enumeratee ao ai m b -> E.Iteratee ai m b -> E.Iteratee ao m b
(=$) = (E.joinI .) . (E.$$)
infixr 0 =$
