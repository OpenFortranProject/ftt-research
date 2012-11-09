{-# LANGUAGE PatternGuards #-}
-- TODO: haddock
module ATerm.Matching
(
  ATermMatcher(..)
, Match(..)
, Binding(..)
, matches
, bindMatches
) where

import ATerm.AbstractSyntax
import Data.Maybe ( isJust, catMaybes )
import Control.Applicative

-- TODO: Make combinators for constructing ATermMatchers

data ATermMatcher = AMAppl  (Match String)  (Match [ATermMatcher])
                  | AMList                  (Match [ATermMatcher])
                  | AMInt   (Match Integer)
                  | AMATerm -- ^ can only be a wildcard
  deriving (Eq, Read, Show, Ord)

data Match a = Exactly a
             | Any
             | Contains a
  deriving (Eq, Read, Show, Ord)

-- TODO: Make some convenience functions for working with Bindings

data Binding = BoundTerm Int     -- ^ The index into the ATermTable of the matching ATerm
             | BoundList [Int]   -- ^ The list of indexes into the ATermTable that match
             | BoundInt  Integer -- ^ The matching Integer
             | BoundStr  String  -- ^ The matching String
             | BoundAppl Int     -- ^ The index into the ATermTable for the ShAAppl term
  deriving (Eq, Read, Show, Ord)

matches :: ATermMatcher -> ATermTable -> Bool
matches m at = isJust (bindMatches m at)

-- The obvious question: Is there a sane way to do with with fewer
-- cases? It's a bit of combinatorial explosion at the moment.
bindMatches :: ATermMatcher -> ATermTable -> Maybe [Binding]
bindMatches = go
  where
  matchChildren ms is t =
    concat <$> sequence (map (\(m,i) -> bindMatches m (getATermByIndex1 i t))
                             (zip ms is))
  containsChildren ms is t =
    concat (catMaybes [ bindMatches m (getATermByIndex1 i t) | i <- is, m <- ms ])
  -- AMATerm cases:
  go AMATerm t = Just [BoundTerm (getTopIndex t)]
  -- AMInt cases:
  go (AMInt Any) t
    | ShAInt i _ <- getATerm t = Just [BoundInt i]
    | otherwise                = Nothing
  go (AMInt (Exactly i)) t
    | ShAInt i' _ <- getATerm t
    , i == i'   = Just [] -- we match, but no wild cards means no binders
    | otherwise = Nothing
  go (AMInt (Contains i)) t -- this doesn't really make much sense, but we allow it
    | ShAInt i' _ <- getATerm t
    , i == i'   = Just [] -- they have to be ints so no wild cards possible
    | otherwise = Nothing
  -- AMList cases:
  go (AMList Any) t
    | ShAList l _ <- getATerm t = Just [BoundList l]
    | otherwise                 = Nothing
  go (AMList (Exactly ms)) t
    | ShAList is _ <- getATerm t
    , length ms == length is = matchChildren ms is t
    | otherwise              = Nothing
  go (AMList (Contains ls)) t
    | ShAList is _ <- getATerm t = Just $ containsChildren ls is t
    | otherwise = Nothing
  -- AMAppl cases:
  go (AMAppl Any Any) t
    | ShAAppl _ _ _ <- getATerm t = Just [BoundAppl (getTopIndex t)]
    | otherwise                   = Nothing
  go (AMAppl (Exactly s) Any) t
    | ShAAppl s' ls _ <- getATerm t
    , s == s'   = Just [BoundList ls]
    | otherwise = Nothing
  go (AMAppl Any (Exactly ms)) t
    | ShAAppl _ is _ <- getATerm t
    , length ms == length is = (BoundAppl (getTopIndex t) :) <$> matchChildren ms is t
    | otherwise              = Nothing
  -- This is the case for strings in the tree
  go (AMAppl (Exactly s) (Exactly [])) t
    | ShAAppl s' [] _ <- getATerm t
    , s == s' = Just [BoundStr s]
  go (AMAppl (Exactly s) (Exactly ms)) t
    | ShAAppl s' is _ <- getATerm t
    , s == s'
    , length ms == length is = matchChildren ms is t
    | otherwise              = Nothing
  go (AMAppl (Contains s) Any) t -- this doesn't really make much sense, but we allow it
    | ShAAppl s' _ _ <- getATerm t
    , s == s'   = Just [] -- they have to be strings so no wild cards possible
    | otherwise = Nothing
  go (AMAppl (Contains s) (Exactly [])) t
    | ShAAppl s' [] _ <- getATerm t
    , s == s'   = Just [] -- they have to be strings so no wild cards possible
    | otherwise = Nothing
  go (AMAppl (Contains s) (Exactly ms)) t
    | ShAAppl s' is _ <- getATerm t
    , s == s'
    , length ms == length is = matchChildren ms is t
    | otherwise              = Nothing
  go (AMAppl Any (Contains ls)) t
    | ShAAppl _ is _ <- getATerm t = Just $ (BoundAppl (getTopIndex t)) : containsChildren ls is t
    | otherwise = Nothing
  go (AMAppl (Exactly s) (Contains ls)) t
    | ShAAppl s' is _ <- getATerm t
    , s' == s   = Just $ containsChildren ls is t
    | otherwise = Nothing
  go (AMAppl (Contains s) (Contains ls)) t -- this doesn't really make sense, but we allow it
    | ShAAppl s' is _ <- getATerm t
    , s == s'   = Just $ containsChildren ls is t
    | otherwise = Nothing
