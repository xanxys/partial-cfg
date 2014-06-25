module CFG2Regex where

import Control.Monad
import qualified Data.Map as Map
import Data.List
import Data.Maybe

data CFG key term = CFG key [(key, CFGClause key term)] deriving(Show, Eq)

data CFGClause key term = CFGClause [[CFGTerm key term]] deriving(Show, Eq)

data CFGTerm key term
	= NonTerminal key
	| Terminal term
	-- internal use
	| RegexTerminal (Regex term)
	deriving(Show, Eq)

-- | Symbol-based regex.
-- (i.e. doesn't contain character specific syntax such as [abc])
data Regex sym
	= RTerminal sym
	| Selection [Regex sym]
	| Sequence [Regex sym]
	| Repetition (Regex sym)
	-- Extensions
	| Optional (Regex sym)
	deriving(Show, Eq)


-- how to detect repetition in recursion
-- find left- or right- form:
-- these are repetition:
-- S = S T1 T2 | T3
-- S = S T1 S | T2
-- S = T1 T2 S | T3 | T4
-- but not these:
-- S = T1 S T2 | T3



-- | Convert CFG to regex (with given expansion depth).
-- Don't pass large depth, since the result size is often exp(depth).
--
-- memo: this is not really useful practically.
-- Each clause should be able to have counter which can be infinite.
--
-- TODO: is there some way to drop dependence on Map (or assoc list)?
convertToRegex :: (Eq k, Ord k) => Int -> CFG k term -> Regex term
convertToRegex depth (CFG root_key clauses_assoc) =
	expandClause clauses depth $ clauses Map.! root_key
	where
		clauses = Map.fromList clauses_assoc

expandClause :: Ord k => Map.Map k (CFGClause k term) -> Int -> CFGClause k term -> Regex term
expandClause dict 0 _ = Selection []  -- doesn't match anything
expandClause dict depth (CFGClause cs) =
	Selection $ map (Sequence . map toTerm) cs
	where
		toTerm (Terminal t) = RTerminal t
		toTerm (NonTerminal k) = expandClause dict (depth - 1) (dict Map.! k)

-- | Remove empty Sequence and Selection. Result can be Nothing if
-- the langueage turns out to be empty.
--
-- Selection [] = {}
-- Sequence [] = {""}
optimizeRegex :: Eq term => Regex term -> Maybe (Regex term)
optimizeRegex (Selection []) = Nothing
optimizeRegex (Sequence []) = Nothing
optimizeRegex (Selection [r]) = optimizeRegex r
optimizeRegex (Sequence [r]) = optimizeRegex r
optimizeRegex (Selection rs) =
	case mapMaybe optimizeRegex rs of
		[] -> Nothing
		rs' -> wrapOptional $ nub rs'
	where
		wrapOptional [] = Nothing
		wrapOptional rs
			|lang0 `elem` rs = Just $ Optional $ flattenSelection $ Selection $ filter (/= lang0) rs
			|otherwise = Just $ flattenSelection $ Selection rs
		lang0 = Sequence []
optimizeRegex (Sequence rs) = do
	rs' <- mapM optimizeRegex rs
	liftM (flattenSequence . Sequence) $  return rs'
optimizeRegex (Repetition r) = liftM Repetition (optimizeRegex r)
optimizeRegex (RTerminal t) = Just (RTerminal t)

flattenSelection = flattenRegex extractIfSelection Selection
	where
		extractIfSelection (Selection rs) = Just rs
		extractIfSelection _ = Nothing

flattenSequence :: Regex term -> Regex term
flattenSequence = flattenRegex extractIfSequence Sequence
	where
		extractIfSequence (Sequence rs) = Just rs
		extractIfSequence _ = Nothing

flattenRegex
	:: (Regex term -> Maybe [Regex term])
	-> ([Regex term] -> Regex term)
	-> Regex term
	-> Regex term
flattenRegex extract unextract r = case extract r of
	Just ls -> unextract $ concatMap (wrapAsList . flattenRegex extract unextract) ls
	Nothing -> r
	where
		wrapAsList r = case extract r of
			Just ls -> ls
			Nothing -> [r]


showRegex :: Regex String -> String
showRegex = showRegexPrec 0

-- | Show regex with minimal amount of parens.
-- precedence:
-- * 10: Optional, Repetition: x?
-- * 5: Sequence: xyz
-- * 3: Selection: |
showRegexPrec :: Int -> Regex String -> String
showRegexPrec d (RTerminal t)
	|elem t ["(", ")", "|", "+", "*", "[", "]", "^", "\\"] = "\\" ++ t
	|otherwise = t
showRegexPrec d (Selection rs) =
	case mapM extractChar rs of
		Nothing ->  wrapParenIf (d > 3) $ intercalate "|" $ map (showRegexPrec 3) rs
		Just chars -> "[" ++ concat chars ++ "]"
	where
		extractChar (RTerminal ch) = Just ch
		extractChar _ = Nothing
showRegexPrec d (Sequence rs) = wrapParenIf (d > 5) $
	concatMap (showRegexPrec 5) rs
showRegexPrec d (Repetition r) = wrapParenIf (d > 10) $
	showRegexPrec 10 r ++ "*"
showRegexPrec d (Optional r) = wrapParenIf (d > 10) $
	showRegexPrec 10 r ++ "?"

wrapParenIf True s = "(" ++ s ++ ")"
wrapParenIf False s = s
