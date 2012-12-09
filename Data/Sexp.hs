{-# LANGUAGE Safe, OverloadedStrings, DefaultSignatures, FlexibleContexts,
             TypeOperators, FlexibleInstances #-}

module Data.Sexp (
        Sexp(..), Sexpable(..),
        escape, unescape
    ) where

import Control.Applicative
import Data.ByteString.Lazy.Char8 as BS hiding ( map, null )
import GHC.Generics

-- | A 'ByteString'-based S-Expression.  You can a lazy 'ByteString'
-- with 'parse'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

class Sexpable a where
    toSexp :: a -> Sexp
    fromSexp :: (Monad m, Applicative m) => Sexp -> m a

    default toSexp :: (Generic a, GSexpable (Rep a)) => a -> Sexp
    toSexp x = fst (gToSexp (from x))

    default fromSexp :: (Generic a, GSexpable (Rep a), Monad m, Applicative m) => Sexp -> m a
    fromSexp x = to <$> gFromSexp x

class GSexpable a where
    gToSexp :: a p -> (Sexp, Bool)
    gFromSexp :: (Monad m, Applicative m) => Sexp -> m (a p)

instance GSexpable U1 where
    gToSexp U1 = (List [], True)

    gFromSexp (List []) = return U1
    gFromSexp _         = fail "not an empty constructor"

instance (GSexpable a, GSexpable b) => GSexpable (a :*: b) where
    gToSexp (x :*: y) =
        let (xs, False) = gToSexp x in
        let (List ys, shouldConcat) = gToSexp y in
        if shouldConcat
        then (List (xs : ys), True)
        else (List [xs, List ys], True)

    gFromSexp (List (x1:x2:xs)) = do
        (:*:) <$> gFromSexp x1 <*> gFromSexp (List (x2:xs))
    gFromSexp _ =
        fail "not a product type"

instance (GSexpable a, GSexpable b) => GSexpable (a :+: b) where
    gToSexp (L1 x) = let (List xs, False) = gToSexp x in (List xs, False)
    gToSexp (R1 x) = let (List xs, False) = gToSexp x in (List xs, False)

    gFromSexp (List xs) = L1 <$> gFromSexp (List xs)
    gFromSexp _         = fail "not a sum type"

instance (GSexpable a, Datatype c) => GSexpable (M1 D c a) where
    gToSexp (M1 x) = gToSexp x
    gFromSexp x = gFromSexp x

instance (GSexpable a, Selector c) => GSexpable (M1 S c a) where
    gToSexp c@(M1 x) =
        let (xs, _) = gToSexp x
        in if null (selName c)
           then (xs, False)
           else (List [Atom (pack (selName c)), xs], False)

    gFromSexp _ = undefined

instance (GSexpable a, Constructor c) => GSexpable (M1 C c a) where
    gToSexp c@(M1 x) =
        case gToSexp x of
            (List [], _) -> (List [Atom (pack (conName c))], False)
            (List xs, _) -> (List [Atom (pack (conName c)), List xs], False)
            _            -> error "constructor case broken in generics reification"

    gFromSexp (List (_:xs)) = M1 <$> gFromSexp (List xs)
    gFromSexp _             = fail "not a constructor"

instance (Sexpable a) => GSexpable (K1 i a) where
    gToSexp (K1 x) = (toSexp x, False)
    gFromSexp x    = K1 <$> fromSexp x

instance Sexpable Bool

instance Sexpable Int where
    toSexp n = Atom (pack (show n))
    fromSexp (Atom s) = return $ read (unpack s)
    fromSexp _        = fail "not an atom"

instance Sexpable Integer where
    toSexp n = Atom (pack (show n))
    fromSexp (Atom s) = return $ read (unpack s)
    fromSexp _        = fail "not an atom"

instance Sexpable Double where
    toSexp n = Atom (pack (show n))
    fromSexp (Atom s) = return $ read (unpack s)
    fromSexp _        = fail "not an atom"

instance Sexpable ByteString where
    toSexp s = Atom (escape s)
    fromSexp (Atom s) = return (unescape s)
    fromSexp _        = fail "not an atom"

instance (Sexpable a, Sexpable b) => Sexpable (a, b) where
    toSexp (x, y) = List [toSexp x, toSexp y]

instance (Sexpable a, Sexpable b, Sexpable c) => Sexpable (a, b, c) where
    toSexp (x, y, z) = List [toSexp x, toSexp y, toSexp z]

instance (Sexpable a, Sexpable b, Sexpable c, Sexpable d) => Sexpable (a, b, c, d) where
    toSexp (x, y, z, t) = List [toSexp x, toSexp y, toSexp z, toSexp t]

instance (Sexpable a) => Sexpable [a] where
    toSexp xs = List (map toSexp xs)

-- | Escape @"@ and @\@ in the given string.  This needs to be done
-- for double-quoted atoms (e.g. @"\"Hello\", he said"@).
escape :: ByteString -> ByteString
escape = BS.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = BS.singleton c

-- | The inverse of 'escape'.
unescape :: ByteString -> ByteString
unescape = BS.reverse . pack . snd . (BS.foldl' unescapeChar (False, []))
  where
    unescapeChar :: (Bool, [Char]) -> Char -> (Bool, [Char])
    unescapeChar (False, cs) '\\' = (True, cs)
    unescapeChar (_, cs) c        = (False, c : cs)
