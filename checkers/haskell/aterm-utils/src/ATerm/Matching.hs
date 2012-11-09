{-# LANGUAGE PatternGuards #-}
-- TODO: haddock
module ATerm.Matching
( -- * Types
  ATermMatcher(..)
, Match(..)
, Binding(..)
-- * Binding and matching
, matches
, bindMatches
-- * Combinators
, exactlyS     -- String -> ATermMatcher
, exactlyI     -- Integer -> ATermMatcher
, exactlyL     -- [ATermMatcher] -> ATermMatcher
, exactlyA     -- String -> Match ATermMatcher] -> ATermMatcher
, contains     -- [ATermMatcher] -> ATermMatcher
, containsL    -- [ATermMatcher] -> ATermMatcher
, containsA    -- Match String -> [ATermMatcher] -> ATermMatcher
, bindT        -- ATermMatcher
, bindL        -- ATermMatcher
, bindI        -- ATermMatcher
, bindS        -- ATermMatcher
, bindA        -- ATermMatcher
) where

import ATerm.AbstractSyntax
import Data.Maybe ( isJust, catMaybes )
import Control.Applicative

-- | While these constructors are exported, the recommend use is by
-- the combinators not direct use.
data ATermMatcher = AMAppl  (Match String)  (Match [ATermMatcher])
                  | AMList                  (Match [ATermMatcher])
                  | AMInt   (Match Integer)
                  | AMATerm -- ^ can only be a wildcard
  deriving (Eq, Read, Show, Ord)

-- | While these constructors are exported, the recommend use is by
-- the combinators not direct use.
data Match a = Exactly a  -- ^ Matching via Eq a, no binding generated on match
             | Any        -- ^ Wildcard, a binding will be created on match
             | Contains a -- ^ Meant for subset matches, use the combinators
  deriving (Eq, Read, Show, Ord)

-- * ATermMatcher Combinators

---------------------------------------------------------------------
-- ** Exact matchers, no binding generated when they match
---------------------------------------------------------------------

-- | Matches exactly the string 's' within the ATerm
exactlyS :: String -> ATermMatcher
exactlyS s = AMAppl (Exactly s) (Exactly [])

-- | Matches exactly the integer 'i' within the ATerm
exactlyI :: Integer -> ATermMatcher
exactlyI i = AMInt (Exactly i)

-- | Matches exactly the list 'xs' within the ATerm
exactlyL :: [ATermMatcher] -> ATermMatcher
exactlyL xs = AMList (Exactly xs)

-- | Matches the string exactly
exactlyA :: String -> Match [ATermMatcher] -> ATermMatcher
exactlyA s ms = AMAppl (Exactly s) ms

---------------------------------------------------------------------
-- ** Partial matchers, ie., they just specify part of the structure
---------------------------------------------------------------------

-- | Matches a partial specification against the children. The
-- matching is maximised so that if the pattern occurs more than
-- once it is matched each time it appears.
contains :: [ATermMatcher] -> Match [ATermMatcher]
contains = Contains

-- | Matches a partial specification of a sub aterm List. The
-- matching is maximised so that if the pattern occurs more than
-- once it is matched each time it appears.
containsL :: [ATermMatcher] -> ATermMatcher
containsL = AMList . Contains

-- | Matches a partial specification of an Appl. The matching is
-- maximised so that if the pattern occurs more than once it is match
-- each time it appears.
containsA :: Match String -> [ATermMatcher] -> ATermMatcher
containsA m ams = AMAppl m (Contains ams)

---------------------------------------------------------------------
-- ** Wildcard matchers, these create bindings when they match
---------------------------------------------------------------------

-- | Matches any ATerm and generates a binding to that term
bindT :: ATermMatcher
bindT = AMATerm

-- | Matches any list and generates a binding to that list
bindL :: ATermMatcher
bindL = AMList Any

-- | Matches any integer and generates a binding to that integer
bindI :: ATermMatcher
bindI = AMInt Any

-- | Matches any string and generates a binding to that string
-- Strings have the form (Appl somestring [] []).
-- Not to be confused with matching the the string part of an Appl.
bindS :: ATermMatcher
bindS = AMAppl Any (Exactly [])

-- | Matches any Appl and generates a binding to that Appl
bindA :: ATermMatcher
bindA = AMAppl Any Any

-- TODO: Make some convenience functions for working with Bindings

data Binding = BoundTerm Int     -- ^ The index into the ATermTable of the matching ATerm
             | BoundList [Int]   -- ^ The list of indexes into the ATermTable that match
             | BoundInt  Integer -- ^ The matching Integer
             | BoundStr  String  -- ^ The matching String
             | BoundAppl Int     -- ^ The index into the ATermTable for the ShAAppl term
  deriving (Eq, Read, Show, Ord)

-- | Tests for a match but discards any bindings
matches :: ATermMatcher -> ATermTable -> Bool
matches m at = isJust (bindMatches m at)

-- The obvious question: Is there a sane way to do this with fewer
-- cases? It's a bit of combinatorial explosion at the moment.
-- | Tests for a match and binds the wildcards in the matcher against
-- the respective terms of the match.
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
  go (AMInt (Exactly i)) t
    | ShAInt i' _ <- getATerm t
    , i == i' = Just [] -- we match, but no wild cards means no binders
  go (AMInt (Contains i)) t -- this doesn't really make much sense, but we allow it
    | ShAInt i' _ <- getATerm t
    , i == i' = Just [] -- they have to be ints so no wild cards possible
  -- AMList cases:
  go (AMList Any) t
    | ShAList l _ <- getATerm t = Just [BoundList l]
  go (AMList (Exactly ms)) t
    | ShAList is _ <- getATerm t
    , length ms == length is = matchChildren ms is t
  go (AMList (Contains ls)) t
    | ShAList is _ <- getATerm t = Just $ containsChildren ls is t
  -- AMAppl cases:
  go (AMAppl Any Any) t
    | ShAAppl _ _ _ <- getATerm t = Just [BoundAppl (getTopIndex t)]
  go (AMAppl (Exactly s) Any) t
    | ShAAppl s' ls _ <- getATerm t
    , s == s' = Just [BoundList ls]
  go (AMAppl Any (Exactly ms)) t
    | ShAAppl _ is _ <- getATerm t
    , length ms == length is = (BoundAppl (getTopIndex t) :) <$> matchChildren ms is t
  -- Matching strings requires this special case
  go (AMAppl (Exactly s) (Exactly [])) t
    | ShAAppl s' [] _ <- getATerm t
    , s == s' = Just [BoundStr s]
  go (AMAppl (Exactly s) (Exactly ms)) t
    | ShAAppl s' is _ <- getATerm t
    , s == s'
    , length ms == length is = matchChildren ms is t
  go (AMAppl (Contains s) Any) t -- this doesn't really make much sense, but we allow it
    | ShAAppl s' _ _ <- getATerm t
    , s == s' = Just [] -- they have to be strings so no wild cards possible
  go (AMAppl (Contains s) (Exactly [])) t
    | ShAAppl s' [] _ <- getATerm t
    , s == s' = Just [] -- they have to be strings so no wild cards possible
  go (AMAppl (Contains s) (Exactly ms)) t
    | ShAAppl s' is _ <- getATerm t
    , s == s'
    , length ms == length is = matchChildren ms is t
  go (AMAppl Any (Contains ls)) t
    | ShAAppl _ is _ <- getATerm t = Just $ (BoundAppl (getTopIndex t)) : containsChildren ls is t
  go (AMAppl (Exactly s) (Contains ls)) t
    | ShAAppl s' is _ <- getATerm t
    , s' == s = Just $ containsChildren ls is t
  go (AMAppl (Contains s) (Contains ls)) t -- this doesn't really make sense, but we allow it
    | ShAAppl s' is _ <- getATerm t
    , s == s' = Just $ containsChildren ls is t
  -- catch all failure case
  go _ _ = Nothing -- Welp, no takers
