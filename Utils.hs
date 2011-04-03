{-# LANGUAGE CPP #-}
module Utils ((=$)) where
import Data.Enumerator

#ifdef OLD_ENUMERATOR
(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
(=$) = (joinI .) . ($$)
infixr 0 =$
#endif